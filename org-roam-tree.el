;;; org-roam-tree.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Brad Stewart
;;
;; Author: Brad Stewart <brad@bradstewart.ca>
;; Maintainer: Brad Stewart <brad@bradstewart.ca>
;; Created: décembre 31, 2025
;; Modified: décembre 31, 2025
;; Version: 0.0.1
;; Keywords: org-roam backlinks tree
;; Homepage: https://github.com/brad/org-roam-tree
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;;  Description
;;  
;; Creates a tree-like backlinks list you can add to your org-roam buffer, which
;; organizes backlinks by their org file source. You can reuse the display
;; logic if you define different groupings and deeper trees, for example, to follow
;; the org tree from the files containing backlinks, or maybe arbitrarily grouped
;; results, which could be helpful with org-roam-ql. As it stands, you can
;; quickly make new display sections. Look at =org-roam-tree-backlinks-section for
;; the pattern, and =org-roam-tree-backlinks= for an example of how to generate
;; the data structure you need.
;;  
;;; Code:

;; Enable by adding org-roam-tree-backlinks-section to org-roam-mode-sections
;; 
;; Show only this section in the org-roam buffer:
;;(setq! org-roam-mode-sections '(org-roam-tree-backlinks-section))
;;(setq! org-roam-mode-sections '(org-roam-tree-reflinks-section))
;;(setq! org-roam-mode-sections '(org-roam-tree-crosslinks-section))
;;
;; Add this section with the others in the org-roam buffer:
;;(add-to-list 'org-roam-mode-sections
;;             #'org-roam-tree-backlinks-section t)
;;
;; DO NOT DO THIS :
;;(setq! org-roam-mode-sections '(org-roam-tree-crosslinks-section org-roam-tree-backlinks-section))
;;having two trees in the same roam buffer is currently somewhat broken. Use at your own risk.
;;

(require 'org-roam)

(defgroup org-roam-tree nil
  "Tree-style display extensions for Org-roam."
  :group 'org-roam)

(defcustom org-roam-tree-default-visible 1
  "Default fold below this level. 0 is top-level groups folded."
  :type 'integer
  :group 'org-roam-tree)


(defvar org-roam-tree-visible-state (make-hash-table :test 'equal)
  "Stores fold states for nodes in multi-level trees.
Keys are of the form (NODE-ID . PATH), where PATH is a vector of child names or node IDs.")

(defun org-roam-tree--node-visible-state (node-id path)
  "Return t if the node at PATH under NODE-ID should be visible.
Defaults to `org-roam-tree-default-visible' if no state stored."
  (gethash (cons node-id path) org-roam-tree-visible-state
           org-roam-tree-default-visible))

(defun org-roam-tree--set-node-visible-state (node path visible)
  "Store visibility state for NODE at PATH."
  (puthash (cons (org-roam-node-id node) path)
           visible
           org-roam-tree-visible-state))

(cl-defun org-roam-tree-backlinks-section (node &key (section-heading "Backlinks:"))
  "A tree-style backlinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-backlinks :section-id 'backlinks-tree))

(cl-defun org-roam-tree-reflinks-section (node &key (section-heading "Reflinks:"))
  "A tree-style backlinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-reflinks :section-id 'reflinks-tree))

(cl-defun org-roam-tree-crosslinks-section (node &key (section-heading "Crosslinks:"))
  "A tree-style backlinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-crosslinks :section-id 'crosslinks-tree))



;;;;;;;;;;;;;;;;;;;; TREE DISPLAY METADATA
;; On building the tree, we store metadata including depth and tree branch parts
;; (is-last for each level) as text properties, but do not immediately decorate,
;; as it is quite an expensive operation. We start the tree folded, and decorate
;; on expand to amortize the display cost.
 
;;;;;; Metadata storage and retrieval functions:

(defconst org-roam-tree--meta-depth 'org-roam-tree-depth)
(defconst org-roam-tree--meta-is-last 'org-roam-tree-is-last)
(defconst org-roam-tree--meta-prefixed 'org-roam-tree-prefixed)
(defconst org-roam-tree--meta-path 'org-roam-tree-path)

(defun org-roam-tree--store-node-metadata (pos depth is-last-vec &optional path)
  "Store tree metadata at POS, the start of a node heading.
PATH is a vector representing the node's position in the tree."
  (add-text-properties
   pos (min (1+ pos) (point-max))
   (list
    org-roam-tree--meta-depth depth
    org-roam-tree--meta-is-last (copy-sequence is-last-vec)
    org-roam-tree--meta-prefixed nil
    org-roam-tree--meta-path path)))

(defun org-roam-tree--get-node-metadata (pos)
  "Return node tree metadata plist stored at POS."
  (list
   :depth    (get-text-property pos org-roam-tree--meta-depth)
   :is-last  (get-text-property pos org-roam-tree--meta-is-last)
   :prefixed (get-text-property pos org-roam-tree--meta-prefixed)
   :path     (get-text-property pos org-roam-tree--meta-path)))

(defun org-roam-tree--message-node-metadata (pos)
  "Message node metadata at POS for debugging."
  (let ((meta (org-roam-tree--get-node-metadata pos)))
    (message "[tree-node] pos=%d depth=%S is-last=%S prefixed=%S path=%S"
             pos
             (plist-get meta :depth)
             (plist-get meta :is-last)
             (plist-get meta :prefixed)
             (plist-get meta :path))))




;;;;;;;;;;;;;;;;;;;; TREE DISPLAY LOGIC
(cl-defun org-roam-tree-section
    (node &key
          (section-heading "Tree Section:")
          (data-getter #'org-roam-tree-backlinks)
          (section-id 'org-roam-tree-section))

  (with-org-roam-tree-layout
   (when-let ((tree (funcall data-getter node)))
     
     (magit-insert-section section-id
       (progn
       (magit-insert-heading section-heading)

       ;; tree is now just a list of top-level nodes
       (let ((count (length tree))
             (is-last-vec (make-vector 8 nil))
             (depth 0))
         (cl-loop for n in tree
                  for idx from 1
                  for lastp = (= idx count) do
                  (aset is-last-vec depth lastp)
                  (org-roam-tree--render-node
                   n
                   (1+ depth)
                   is-last-vec)))))))
(run-at-time 0.05 nil #'org-roam-tree--apply-folded-state)
  )

(defun org-roam-tree--render-node (node depth is-last-vec &optional parent-path)
  (let* ((value    (if (consp node) (car node) node))
         (children (when (consp node) (cdr node)))
         (leafp    (not (and children (listp children))))
         (start (point))
         (section-id
          (cond
           ((stringp value)
            (intern (concat "org-roam-tree-file-"
                            (file-name-nondirectory value))))
           ((org-roam-backlink-p value)
            'org-roam-tree-backlink)
           ((org-roam-reflink-p value)
            'org-roam-tree-reflink)
           (t
            'org-roam-tree-node)))
(node-id-or-name (cl-typecase value
                           (org-roam-backlink
                            (org-roam-node-id (org-roam-backlink-source-node value)))
                           (org-roam-reflink
                            (org-roam-node-id (org-roam-reflink-source-node value)))
                           (string
                            (file-name-nondirectory value))))
(parent-path (or parent-path ""))
       (path (concat parent-path "-" node-id-or-name)))

    (magit-insert-section section-id value
      ;; Insert this node’s content
      (org-roam-tree--insert-leaf value children)

      ;; store tree metadata at node start
      (org-roam-tree--store-node-metadata start depth is-last-vec path)
      ;(org-roam-tree--message-node-metadata start)

      ;; Prefix the node
      (save-excursion
        (goto-char start)
        (org-roam-tree--prefix-node-content  depth))

      
      ;; Recurse into children *inside* the section
      (unless leafp
        (let ((count (length children)))
          (cl-loop for child in children
                   for idx from 1
                   for lastp = (= idx count) do
                   (aset is-last-vec depth lastp)
                   (org-roam-tree--render-node
                    child
                    (1+ depth)
                    is-last-vec
                    path))))
)))

(defun org-roam-tree--insert-leaf (value children)
  (cl-typecase value
    (org-roam-backlink
     (org-roam-node-insert-section
      :source-node (org-roam-backlink-source-node value)
      :point (org-roam-backlink-point value)
      :properties (org-roam-backlink-properties value)

      
     ))
    (org-roam-reflink
     (when-let ((pt (org-roam-reflink-point value)))
       (org-roam-node-insert-section
        :source-node (org-roam-reflink-source-node value)
        :point pt
        :properties (org-roam-reflink-properties value)

        ))
     )
    (string
     (magit-insert-heading (format "%s (%d)" (file-name-nondirectory value) (length children))))))




(defun org-roam-tree--apply-folded-state ()
  "Walk the Org-roam tree buffer and fold sections based on stored metadata."
    (with-current-buffer (get-buffer "*org-roam*")
    ;(magit-section-show-level-4-all)
    (save-excursion
      (goto-char (point-min))

                  (vertical-motion 1)
      (while (and (not (eobp))
                  (not (eq (magit-current-section) magit-root-section)))

        (when-let ((sec (magit-current-section)))
          (magit-section-show-children sec))

        (when (get-text-property (point) org-roam-tree--meta-depth)
          (let* ((meta (org-roam-tree--get-node-metadata (point)))
                 (node-id (org-roam-node-id org-roam-buffer-current-node))
                 (path (plist-get meta :path))
                 (depth (plist-get meta :depth))
                 (visible (org-roam-tree--node-visible-state node-id path)))
            (if
                (and
                 (if (booleanp visible)
                     visible
                   (> depth visible))
                 (not (magit-section-hidden (magit-current-section))))
                     
                (progn
                  (forward-char (* depth 3)) ; make sure we're in the section
                (magit-section-hide (magit-current-section)))
              )))
        (magit-section-forward)
        ))))


(defmacro with-org-roam-tree-layout (&rest body)
  "Ensure proper visual layout for Org-roam tree rendering.

- Selects the Org-roam buffer window.
- Temporarily adds a right margin for tree prefixes to avodi a race
  condition between inserting buffer prefixes and visual-line reflow
- Restores the original margins afterward.

BODY is the code that renders the tree content."
  `(with-selected-window (get-buffer-window org-roam-buffer)
     (save-excursion
     (let ((old-margin (window-margins)))  ; save existing margins
       (unwind-protect
           (progn
             ;; Add 6 columns to the right margin for tree prefixes
             ;; TODO : for future iterations with greater depth trees, calculate the
             ;; margin width.
             (set-window-margins (selected-window)
                                 (car old-margin)
                                 (+ (or (cdr old-margin) 0) 6))
             ,@body)
         ;; Restore original margins
         (set-window-margins (selected-window)
                             (car old-margin)
                             (cdr old-margin)))))))
(defun org-roam-tree--track-toggle (&rest _args)
  "Save fold state for the section just toggled."
  (when-let ((section (magit-current-section)))
    (let*
              ((node org-roam-buffer-current-node)
              (start (oref section start))
              (path (get-text-property start org-roam-tree--meta-path))
              (hidden (not (org-roam-tree--node-visible-state (org-roam-node-id node) path))))

    ;; Store state: visible = not hidden
    (org-roam-tree--set-node-visible-state node path hidden))))

(advice-add 'magit-section-toggle :after #'org-roam-tree--track-toggle)

(defun org-roam-tree--prefix-node-content (depth)
  "Insert tree prefixes for a node's rendered content.

Assumes point is at the beginning of the node. Uses metadata
stored at point to determine depth and is-last-vec. Marks node
as prefixed to avoid duplication."
  (let* ((meta (org-roam-tree--get-node-metadata (point)))
         (is-last-vec (plist-get meta :is-last))
         (prefixed (plist-get meta :prefixed))
         (path (plist-get meta :path))
         (start (point)))
    ;; Already prefixed? skip
    (unless prefixed
      ;; Mark as prefixed
      (add-text-properties start (1+ start)
                           `(,org-roam-tree--meta-prefixed t))

      ;; First visual line
      (insert (org-roam-tree-make-prefix depth t is-last-vec))

                                        ; move metadata to new beginning of line
      (remove-text-properties
       (point) (1+ (point))
       (list org-roam-tree--meta-depth nil
             org-roam-tree--meta-is-last nil
             org-roam-tree--meta-prefixed nil))
                                        ; org-roam-tree--meta-path nil)) -- leave this one for easier lookup on fold
      (org-roam-tree--store-node-metadata start depth is-last-vec path)

      ;; Subsequent visual lines, stop at next node or eobp
      (let ((last-point -1))
        (while (and (not (eobp))
                    (or (not (get-text-property (point) org-roam-tree--meta-depth))
                        (= (point) start))
                    (or (= (point) start) (/= (point) last-point)))
          (vertical-motion 1) ;; much faster than line-move-visual,
                              ;; but requires manual point tracking
          (beginning-of-visual-line)
          (setq last-point (point))
          (unless (string-match-p "^\\([ |]*│[ |]*\\)$"
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))
            ;; don't re-prefix lines that are only prefixes...
            ;; TODO bug - there is a line with an extra | after the section. Tried
            ;; (if (magit-current-section) ...) instead of the regex,
            ;; but that didn't prefix any lines

            (unless (eq (char-before) ?\n)
              (insert "\n")) ;; convert visual wraps to hard newlines
            
            ;; insert the prefix, ensuring we're not adding empty prefixes to
            ;; empty lines : stops occasional infinite loops.
            (let ((prefix (org-roam-tree-make-prefix depth nil is-last-vec))
                  (line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (unless (cl-loop for c across (concat prefix line-text) always (eq c ?\s))
                (insert prefix)))
            ))))))



(defun org-roam-tree-make-prefix (depth is-node is-last)
  "Generate a tree-style prefix string for a line.

DEPTH is the nesting level (1 = file).
IS-NODE is t if this is a child node.
IS-LAST may be a boolean or a list of booleans indicating whether
each depth level is the last sibling. If boolean, expand to a list of
(nil nil nil ... is-last); so assumes only the deepest level is specified
and the branch is not last at any other level"
  ;; TODO don't make a vector, just have a second branch that returns "|  " for
  ;; all but last.
  ;; 

  ;; vertical guides for ancestor levels
  (let ((prefix ""))
        (dotimes (i  depth)
          (setq prefix
                (concat prefix
                        (cond
                         ( (and is-node (aref is-last i) (= i (1- depth)) )  "└─ ")
                          ((and is-node (= i (1- depth)))"├─ ")
                          ((not (aref is-last i)) "│  ")
                          (t "   ")))))
    
    prefix))

(defun org-roam-tree-backlinks (&optional node)
  "Return backlinks of NODE grouped by source file.

Return value:
  ((FILE . (BACKLINK BACKLINK ...)) ...)

NODE defaults to `org-roam-node-at-point` if nil."
  (let* ((node (or node (org-roam-node-at-point)))
         (backlinks (org-roam-backlinks-get node :unique t))
         (table (make-hash-table :test 'equal)))
    (dolist (bl backlinks)
      (let* ((src (org-roam-backlink-source-node bl))
             (file (org-roam-node-file src)))
        (when src
          (puthash file
                   (cons bl (gethash file table))
                   table))))
    (let (result)
      (maphash
       (lambda (file backlinks)
         (push (cons file (nreverse backlinks)) result))
       table)
      result)))

(defun org-roam-tree-reflinks (&optional node)
  "Return reflinks of NODE grouped by source file.

Return value:
  ((FILE . (REFLINK REFLINK ...)) ...)

NODE defaults to `org-roam-node-at-point` if nil."
  (let* ((node (or node (org-roam-node-at-point)))
         (reflinks (org-roam-reflinks-get node ))
         (table (make-hash-table :test 'equal)))
    (dolist (rl reflinks)
      (let* ((src (org-roam-reflink-source-node rl))
             (file (org-roam-node-file src)))
        (when src
          (puthash file
                   (cons rl (gethash file table))
                   table))))
    (let (result)
      (maphash
       (lambda (file reflinks)
         (push (cons file (nreverse reflinks)) result))
       table)
      result)))

(defun org-roam-tree-crosslink-query (node-id)
"Return a list of triples for nodes two hops from NODE-ID.

Each element is of the form:

  (CROSSLINK-ID BACKLINK-ID BACKLINK-OBJ)

- CROSSLINK-ID: the ID of a node that is linked to by one of NODE-ID’s backlinks.
- BACKLINK-ID: the ID of a node that links to NODE-ID.
- BACKLINK-OBJ: a fully populated `org-roam-backlink` object representing
  the backlink from BACKLINK-ID to NODE-ID.

The list is ordered descending by how many of NODE-ID’s backlinks link
to each CROSSLINK-ID (i.e., nodes linked to by multiple backlinks appear first)."
  (let* ((db (org-roam-db))
         ;; wrap node-id in quotes for SQLite storage
         (quoted-id (format "\"%s\"" node-id))
         ( query (format "SELECT DISTINCT
       crosslinks.dest AS crosslink_id,
       SUM(1) OVER (PARTITION BY crosslinks.dest) AS crosslink_count,
       backlinks.source AS backlink_id,
       backlinks.*
       FROM links AS backlinks
       JOIN links AS crosslinks
         ON backlinks.source = crosslinks.source
       WHERE backlinks.dest = '\"%s\"'
         AND crosslinks.type = '\"id\"'
         AND crosslinks.dest != '\"%s\"'
       ORDER BY crosslink_count desc;"
                         node-id node-id))
         (results
          (emacsql db
                   query
                   )))
    ;; Map each row (dest count) to a cons
    (mapcar
     (lambda (row)
       (let* ((crosslink-id (nth 0 row))
              (backlink-id  (nth 2 row))
              (point        (nth 3 row))
              (source-id    (nth 4 row))
              (dest-id      (nth 5 row))
              (props        (nth 7 row))
              (bo (org-roam-backlink-create
                   :source-node (org-roam-node-from-id source-id)
                   :target-node (org-roam-node-from-id dest-id)
                   :point point
                   :properties props)))
         (list crosslink-id backlink-id bo)))
     results)))
(defun org-roam-tree-crosslinks (&optional node)
  "Return crosslinks of NODE as a 3-level tree:
((CROSSLINK-TITLE
   (FILE . (BACKLINK BACKLINK ...))
   (FILE . (BACKLINK BACKLINK ...)))
 ...)"
  (let* ((node (or node (org-roam-node-at-point)))
         (crosslink-rows (org-roam-tree-crosslink-query (org-roam-node-id node)))
         (table (make-hash-table :test 'equal)))
    ;; Build a table: crosslink-id → (file → list of backlinks)
    (dolist (row crosslink-rows)
      (cl-destructuring-bind (crosslink-id backlink-id bo) row
        (let* ((backlink-node (org-roam-node-from-id backlink-id))
               (file (when backlink-node (org-roam-node-file backlink-node)))
               (file-table (or (gethash crosslink-id table)
                               (make-hash-table :test 'equal)))
               (bls (gethash file file-table)))
          (when file
            (puthash file (cons bo bls) file-table)
            (puthash crosslink-id file-table table)))))
    ;; Convert hash tables to nested lists with file names and backlinks
    (let (result)
      (maphash
       (lambda (crosslink-id file-table)
         (let (files)
           (maphash
            (lambda (file bls)
              (push (cons file (nreverse bls)) files))
            file-table)
           (let ((crosslink-node (org-roam-node-from-id crosslink-id)))
             (push (cons (if crosslink-node
                             (org-roam-node-title crosslink-node)
                           crosslink-id) ;;; occasional nil errors unless I do this
                         (nreverse files))
                   result))))
       table)
      (nreverse result))))




;;;;;;;;;;;;;;;;;;;; MENU BUTTON
;; Menu to quickly change the roam buffer sections

(defmacro org-roam-tree--make-button (label fn &rest props)
  "Create a header-line button with LABEL that runs FN after ensuring window focus."
  `(propertize ,label
               'mouse-face 'highlight
               'help-echo ,(plist-get props :help)
               'local-map (let ((m (make-sparse-keymap)))
                            (define-key m [header-line down-mouse-1]
                              (lambda (event)
                                (interactive "e")
                                (org-roam-tree--helper-ensure-buffer-focus event ,fn)))
                            m)))

(setq org-roam-tree--header-buttons
  (list
   (org-roam-tree--make-button "" #'org-roam-tree--header-menu
                          ;:help "Menu")
   )))

(defun org-roam-tree--helper-ensure-buffer-focus (event fn &rest args)
  "Ensure the clicked window is selected, then call FN with ARGS."
  (interactive "e")
  (let ((win (posn-window (event-start event))))
    (select-window win))
  (apply fn args))

(setq org-roam-tree--roam-sections-cookie org-roam-mode-sections)

(defun org-roam-tree--header-menu ()
  (popup-menu
   '("menu"
     ["Default section" (org-roam-tree--change-sections org-roam-tree--roam-sections-cookie)]
     ["Backlinks tree" (org-roam-tree--change-sections '(org-roam-tree-backlinks-section))]
     ["Reflinks tree" (org-roam-tree--change-sections '(org-roam-tree-reflinks-section))]
     ["Crosslinks tree" (org-roam-tree--change-sections '(org-roam-tree-crosslinks-section))])))

(defun org-roam-tree--change-sections (sections)
  "Change the sections displayed in org-roam buffer to sections and
reload."
  (message "Changing roam buffer sections...")
  (setq org-roam-mode-sections sections)
  (org-roam-buffer-refresh))

(defun org-roam-tree--add-header-buttons ()
  (let* ((title (propertize
                 (org-roam-node-title org-roam-buffer-current-node)
                 'face 'bold))
         (btn-list org-roam-tree--header-buttons)
         (btn-width ;; Compute total width for right alignment
          (apply #'+
                 (mapcar (lambda (btn)
                           (+ 2 (string-width btn))) ; +1 for space
                         btn-list))))
    (setq header-line-format
          `(
            ,title
            ;; Flexible space before the buttons
            (:eval (propertize
                    " "
                    'display '((space :align-to (- right ,btn-width)))))

            ;; Insert each button followed by one space
            ,@(cl-mapcan (lambda (btn) (list btn " "))
                         btn-list)))))

(add-hook 'org-roam-buffer-postrender-functions
          #'org-roam-tree--add-header-buttons)

(provide 'org-roam-tree)
;;; org-roam-tree.el ends here
