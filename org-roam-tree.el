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
;;(setq! org-roam-mode-sections '(org-roam-tree-unlinked-references-section))
;;
;; Add this section with the others in the org-roam buffer:
;;(add-to-list 'org-roam-mode-sections
;;             #'org-roam-tree-backlinks-section t)
;;
;; You can have multiple sections, with caveats :
;; 
;;(setq! org-roam-mode-sections '(org-roam-tree-crosslinks-section org-roam-tree-backlinks-section))
;;(setq! org-roam-mode-sections '(org-roam-tree-unlinked-references-section org-roam-tree-backlinks-section))
;;
;;having two trees in the same roam buffer currently has a couple of bugs. The
;;trees will display in the opposit order that they are listed, and there are
;;some rendering abnormalities with the tree prefexis. It seems functional, but
;;there is still work to do.
;;

(require 'org-roam)

(defgroup org-roam-tree nil
  "Tree-style display extensions for Org-roam."
  :group 'org-roam)

(defcustom org-roam-tree-default-visible 1
  "Default fold below this level. 0 is top-level groups folded.
  2 will show node content on most trees."
  :type 'integer
  :group 'org-roam-tree)

(defcustom org-roam-tree-quote-source-prefix "SOURCE:"
  "Prefix inserted before the source link when copying a quote."
  :type 'string
  :group 'org-roam-tree)

(defcustom org-roam-tree-quote-target-predicate #'org-roam-tree--org-mode-buffer-p
  "Predicate function used to filter candidate buffers for
`org-roam-tree-quote-to-buffer'. Called with a buffer, no
arguments (buffer is current when called). Should return non-nil
if the buffer is a valid copy target.

Set this to `#'always' to allow copying to any open buffer,
regardless of major mode."
  :type 'function
  :group 'org-roam-tree)

(defcustom org-roam-tree-auto-refresh-buffer nil
  "If non-nil, automatically refresh the org-roam buffer after
converting an unlinked reference to a backlink, or removing a
backlink. If nil, the buffer is not refreshed (much faster when
processing many links in a row); the affected entry is instead
marked in place -- underlined for a new link, struck through for
a removed one -- until the buffer is next refreshed."
  :type 'boolean
  :group 'org-roam-tree)

(defcustom org-roam-tree-sort-mode 'hits
  "How to sort file-level groups in tree sections.

Possible values:
  `hits'    — sort descending by number of hits in the file (default).
  `recency' — sort descending by file modification time (most recent first).
  `name'    — sort ascending by file base-name."
  :type '(choice (const :tag "Hits (most matches first)" hits)
                 (const :tag "Recency (most recently modified first)" recency)
                 (const :tag "Name (alphabetical)" name))
  :group 'org-roam-tree)

(defface org-roam-tree-added-face
  '((t :underline t))
  "Face marking a tree entry just converted to a backlink, when
`org-roam-tree-auto-refresh-buffer' is nil."
  :group 'org-roam-tree)

(defface org-roam-tree-removed-face
  '((t :strike-through t))
  "Face marking a tree entry whose backlink was just removed, when
`org-roam-tree-auto-refresh-buffer' is nil."
  :group 'org-roam-tree)

(defun org-roam-tree--org-mode-buffer-p (buffer)
  (with-current-buffer buffer
    (derived-mode-p 'org-mode)))

(defvar org-roam-tree--quote-target-history nil
  "List of buffer names previously used as quote-copy targets, most
recent first.")


(defvar org-roam-tree-visible-state (make-hash-table :test 'equal)
  "Stores fold states for nodes in multi-level trees.
Keys are of the form (NODE-ID . PATH), where PATH is a vector of child names or node IDs.")
(defvar-local org-roam-tree--prefixed-lines-count nil)

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

;;;;;;;;;;;;;;;;;; BACKLINK BUFFER SECTIONS
;; here are the included buffer sections. You can implement others following
;; this pattern. The bulk of the work is done by the :data-getter function,
;; which should return a list of cons cells,  of the format
;; (SECTION . (CHILDREN ...)) where nodes are a string, filename, a backlink or
;; a reflink. Some examples from the provided data getters:
;;  ((FILE . (BACKLINK BACKLINK ...)) ...)
;;  ((CROSSLINK-TITLE .
;;   ((FILE . (BACKLINK BACKLINK ...))
;;    (FILE . (BACKLINK BACKLINK ...)))) ...)
(cl-defun org-roam-tree-backlinks-section (node &key (section-heading "Backlinks:"))
  "A tree-style backlinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-backlinks :section-id 'backlinks-tree))

(cl-defun org-roam-tree-reflinks-section (node &key (section-heading "Reflinks:"))
  "A tree-style reflinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-reflinks :section-id 'reflinks-tree))

(cl-defun org-roam-tree-unlinked-references-section (node &key (section-heading "Unlinked References:"))
  "A tree-style unlinked references section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-unlinked-references :section-id 'unlinked-references-tree))

(cl-defun org-roam-tree-crosslinks-section (node &key (section-heading "Crosslinks:"))
  "A tree-style crosslinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-crosslinks :section-id 'crosslinks-tree))

(cl-defun org-roam-tree-search-string-section (node &key search-string)
  "A tree-style section that searches for an arbitrary SEARCH-STRING
across the org-roam directory.

When SEARCH-STRING is nil the user is prompted interactively.
The section heading reflects the query used."
  (let* ((query (or search-string
                    (read-string "Search org-roam for: ")))
         (heading (format "Search \"%s\":" query)))
    (org-roam-tree-section node
                           :section-heading heading
                           :data-getter (lambda (_node)
                                          (org-roam-tree-search-string query))
                           :section-id 'search-string-tree)))


(defun org-roam-tree--render-search (query)
  "Render the org-roam buffer with a search section for QUERY.
A nil-safe alternative to `org-roam-buffer-render-contents' for use
when `org-roam-buffer-current-node' may be nil (i.e. when the buffer
is opened without prior node navigation).  On subsequent navigation
renders, the upstream function runs normally with a valid node."
  (let* ((inhibit-read-only t)
         (dir (or (bound-and-true-p org-roam-buffer-current-directory)
                  org-roam-directory)))
    (erase-buffer)
    (org-roam-mode)
    (setq-local default-directory dir)
    (setq-local org-roam-directory dir)
    ;; Only call org-roam-node-title when we actually have a node;
    ;; --add-header-buttons will set "Search: QUERY" via the postrender hook.
    (when org-roam-buffer-current-node
      (org-roam-buffer-set-header-line-format
       (org-roam-node-title org-roam-buffer-current-node)))
    (magit-insert-section (org-roam)
      (magit-insert-heading)
      (org-roam-tree-search-string-section org-roam-buffer-current-node
                                           :search-string query))
    (run-hooks 'org-roam-buffer-postrender-functions)
    (goto-char 0)))

;;;###autoload
(defun org-roam-tree-search (query)
  "Open the org-roam buffer and display search results for QUERY.

QUERY is searched as a literal word-boundary pattern across all files
in `org-roam-directory'.  When called interactively, prompt for QUERY.
May also be called non-interactively with QUERY as a string.

The org-roam buffer is created and displayed if it is not already
visible.  Subsequent calls replace the previous search results."
  (interactive "sSearch org-roam for: ")
  (when (and query (not (string-empty-p query)))
    (setq org-roam-mode-sections
          (list (list #'org-roam-tree-search-string-section
                      :search-string query)))
    (let ((buf (get-buffer-create org-roam-buffer)))
      (display-buffer buf)
      (with-current-buffer buf
        (org-roam-tree--render-search query)))))

(cl-defun org-roam-tree-section
    (node &key
          (section-heading "Tree Section:")
          (data-getter #'org-roam-tree-backlinks)
          (section-id 'org-roam-tree-section))
    "The generic section. Just add water."

  (setq org-roam-tree--prefixed-lines-count 0)
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
                   is-last-vec))))))))



;;;;;;;;;;;;;;;;;;;; TREE DISPLAY METADATA
;; On building the tree, we store metadata including depth and tree branch parts
;; (is-last for each level) as text properties, but do not immediately decorate,
;; as it is quite an expensive operation. We start the tree folded, and decorate
;; on expand to amortize the display cost.
 
;;;;;; Metadata storage and retrieval functions:

(defconst org-roam-tree--meta-depth 'org-roam-tree-depth)
(defconst org-roam-tree--meta-is-last 'org-roam-tree-is-last)
(defconst org-roam-tree--meta-path 'org-roam-tree-path)
(defconst org-roam-tree--meta-is-prefix-string 'org-roam-tree-prefix)

(defun org-roam-tree--store-node-metadata (pos depth is-last-vec &optional path)
  "Store tree metadata at POS, the start of a node heading.
PATH is a vector representing the node's position in the tree."
  (add-text-properties
   pos (min (1+ pos) (point-max))
   (list
    org-roam-tree--meta-depth depth
    org-roam-tree--meta-is-last (copy-sequence is-last-vec)
    org-roam-tree--meta-path path)))

(defun org-roam-tree--get-node-metadata (pos)
  "Return node tree metadata plist stored at POS."
  (list
   :depth    (get-text-property pos org-roam-tree--meta-depth)
   :is-last  (get-text-property pos org-roam-tree--meta-is-last)
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



;;;;;;;; simlinks: structure for simulating "links" that we construct in
;; non-roam ways. Can be reused for a nice looking view for your own created
;; "links". This needs to be defined above where it's used.
(cl-defstruct org-roam-tree-simlink
  file      ;; full path to file
  title     ;; usually filename or a short description
  row       ;; line number in file
  col       ;; column number in file
  point     ;; optional character position; if nil, row/col is used - TODO
  body      ;; the text content to render (string, can have text properties)
  match     ;; the text matching our search regex
  properties) ;; any extra metadata as a plist - TODO


;;;;;;;;;;;;;;;;;;;; TREE DISPLAY LOGIC

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
           ((org-roam-tree-simlink-p value)
            'org-roam-tree-simlink)
           (t
            'org-roam-tree-node)))
(node-id-or-name (cl-typecase value
                           (org-roam-backlink
                            (org-roam-node-id (org-roam-backlink-source-node value)))
                           (org-roam-reflink
                            (org-roam-node-id (org-roam-reflink-source-node value)))
                           (org-roam-tree-simlink
                            (org-roam-tree-simlink-title value))
                           (string
                            (file-name-nondirectory value))))
(parent-path (or parent-path ""))
       (path (concat parent-path "-" node-id-or-name)))

    (magit-insert-section section-id value
      ;; Insert this node’s content
      (org-roam-tree--insert-leaf value children)

      ;; store tree metadata at node start
      (org-roam-tree--store-node-metadata start depth is-last-vec path)

      (when (< org-roam-tree--prefixed-lines-count (window-body-height))
        ;;Prefix the immediately visible nodes. Do the rest lazily.
        (save-excursion
          (goto-char start)
          (setq org-roam-tree--prefixed-lines-count (+ org-roam-tree--prefixed-lines-count (org-roam-tree--prefix-node-content)))))

      
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
                    path)))))))


(defun org-roam-tree--insert-leaf (value children)
  (cl-typecase value
    (org-roam-backlink
     (let ((start (point)))
       (org-roam-node-insert-section
        :source-node (org-roam-backlink-source-node value)
        :point (org-roam-backlink-point value)
        :properties (org-roam-backlink-properties value))
       (put-text-property start (point) 'keymap org-roam-tree-backlink-map)
       (put-text-property start (point) 'org-roam-tree-leaf-value value)))
    (org-roam-reflink
     (when-let ((pt (org-roam-reflink-point value)))
       (let ((start (point)))
         (org-roam-node-insert-section
          :source-node (org-roam-reflink-source-node value)
          :point pt
          :properties (org-roam-reflink-properties value))
         (put-text-property start (point) 'keymap org-roam-tree-backlink-map)
         (put-text-property start (point) 'org-roam-tree-leaf-value value))))
    (org-roam-tree-simlink
     (let ((start (point)))
       (org-roam-tree-simlink-insert-section value)
       (put-text-property start (point) 'org-roam-tree-leaf-value value)))
    (string
     (magit-insert-heading (format "%s (%d)" (file-name-nondirectory value) (length children))))))

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

(defun org-roam-tree--jit-prefix-range (start end)
  "Prefix all un-prefixed lines between START and END.

Snaps START to the nearest node boundary, then walks visual lines
until END, calling `org-roam-tree--prefix-node-content' at each
node start whose :prefixed metadata is missing."
  (let ((needed-prefixing nil)
        node-start-pos)
  (with-org-roam-tree-layout
      (let ((inhibit-read-only t))
      ;; ---- snap START to a node boundary ----
      (goto-char start)
      (beginning-of-line)
      (while (and (> (point) (point-min))
                  (not (get-text-property (point) org-roam-tree--meta-depth)))
        (forward-line -1)
        (beginning-of-line))

      ;; ---- walk until END ----
      (let ((limit end)
            (last-point -1))
        (while (and (not (eobp))
                    (< (point) limit)
                    (/= (point) last-point))
          (setq last-point (point))

          ;; track start
          (when (get-text-property (point) org-roam-tree--meta-depth)
            (setq node-start-pos (point)))

          (unless (get-text-property (point) org-roam-tree--meta-is-prefix-string)
            (save-excursion ;;;###
              (goto-char node-start-pos)
              (setq needed-prefixing (or needed-prefixing (> 0 
                                                               (org-roam-tree--prefix-node-content))))))
          ;; move by visual lines
          (forward-line 1)
          (beginning-of-line)))))
  needed-prefixing))

(defun org-roam-tree--active-p ()
  "Return non-nil if an org-roam-tree section is active.
Handles both bare symbol form (e.g. `org-roam-tree-backlinks-section')
and list form (e.g. `(org-roam-tree-search-string-section :search-string \"foo\")')."
  (and (boundp 'org-roam-mode-sections)
       (cl-some (lambda (s)
                  (let ((fn (cond ((symbolp s) s)
                                  ((consp s)  (car s)))))
                    (and (symbolp fn)
                         (string-prefix-p "org-roam-tree"
                                          (symbol-name fn)))))
                org-roam-mode-sections)))

(defun org-roam-tree--jit-prefix (start end)
  (message "jit-prefix called: start=%s end=%s" start end)
  (when (and (derived-mode-p 'org-roam-mode)
             (org-roam-tree--active-p))
    (if (org-roam-tree--jit-prefix-range start end) ;; t if made changes
        (let ((range-end (min (+ start (* 6 (- end start))) (point-max))))
          ;; batch ahead for responsiveness
          (message "beep")
          (org-roam-tree--jit-prefix-range end range-end)
(force-window-update (get-buffer-window org-roam-buffer))
          )
      )))

(add-hook 'org-roam-mode-hook
          (lambda ()
            (jit-lock-register #'org-roam-tree--jit-prefix)))

(defun org-roam-tree--prefix-node-content ()
  "Insert tree prefixes for a node's rendered content.

Assumes point is at the beginning of the node. Uses metadata
stored at point to determine depth and is-last-vec. Marks node
as prefixed to avoid duplication."
  (let* ((meta (org-roam-tree--get-node-metadata (point)))
         (is-last-vec (plist-get meta :is-last))
         (prefixed (plist-get meta :prefixed))
         (path (plist-get meta :path))
         (depth (plist-get meta :depth))
         (start (point))
         (lines 0))

      ;; First visual line
      (unless (get-text-property (line-beginning-position)
                                 org-roam-tree--meta-is-prefix-string)
        (insert (org-roam-tree-make-prefix depth t is-last-vec))
        (setq lines 1))
      (remove-text-properties ; move metadata to new beginning of line
       (point) (1+ (point))
       (list org-roam-tree--meta-depth nil
             org-roam-tree--meta-is-last nil))
                                        ; org-roam-tree--meta-path nil)) -- leave this one for easier lookup on fold
      (org-roam-tree--store-node-metadata start depth is-last-vec path)

      ;; Subsequent visual lines, stop at next node or eobp
      (let ((last-point -1))

        (vertical-motion 1) ;; much faster than line-move-visual,
        ;; but requires manual point tracking
        (while (and (not (eobp))
                    (or (not (get-text-property (point) org-roam-tree--meta-depth))
                        (= (point) start))
                    (or (= (point) start) (/= (point) last-point)))
          (beginning-of-visual-line)
          (setq last-point (point))
          
          (unless (eq (char-before) ?\n)

            (insert (propertize "\n" org-roam-tree--meta-is-prefix-string t))
            (setq lines (1+ lines))
            ) ;; convert visual wraps to hard newlines
          
          ;; insert the prefix, ensuring we're not adding empty prefixes to
          ;; empty lines : stops occasional infinite loops.
          (unless
              (get-text-property (line-beginning-position) org-roam-tree--meta-is-prefix-string)
            (let ((prefix (org-roam-tree-make-prefix depth nil is-last-vec))
                  (line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (unless (cl-loop for c across (concat prefix line-text) always (eq c ?\s))
                (insert prefix))))

          (vertical-motion 1) ;; much faster than line-move-visual,
          ;; but requires manual point tracking
          ))
      lines))

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
    
        (setq prefix (propertize prefix org-roam-tree--meta-is-prefix-string t))
    prefix))




;;;;;;;;;;;;;;;;;;;; Sort helpers

(defun org-roam-tree--set-sort-mode (mode)
  "Set `org-roam-tree-sort-mode' to MODE and refresh the org-roam buffer."
  (setq org-roam-tree-sort-mode mode)
  (when (get-buffer org-roam-buffer)
    (org-roam-buffer-refresh)))

(defun org-roam-tree--file-mtime (file)
  "Return the modification time of FILE as a float, or 0 if unavailable."
  (let ((attrs (file-attributes file)))
    (if attrs
        (float-time (file-attribute-modification-time attrs))
      0)))

(defun org-roam-tree--sort-file-groups (groups)
  "Sort a list of (FILE . CHILDREN) GROUPS according to `org-roam-tree-sort-mode'.

In-file ordering of CHILDREN is preserved; only the top-level file
groups are reordered.

Sort modes:
  `hits'    — descending by number of children (most matches first).
  `recency' — descending by file modification time (newest first).
  `name'    — ascending by file base-name."
  (cl-case org-roam-tree-sort-mode
    (hits
     (sort groups (lambda (a b)
                    (> (length (cdr a)) (length (cdr b))))))
    (recency
     (sort groups (lambda (a b)
                    (> (org-roam-tree--file-mtime (car a))
                       (org-roam-tree--file-mtime (car b))))))
    (name
     (sort groups (lambda (a b)
                    (string< (file-name-nondirectory (car a))
                             (file-name-nondirectory (car b))))))
    (t groups)))

;;;;;;;;;;;;;;;;;;;; Sections content definitions
;; These are logic for selecting and structuring node trees to
;; display.

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
     (push (cons file (sort (nreverse backlinks)
                            (lambda (a b)
                              (< (org-roam-backlink-point a)
                                 (org-roam-backlink-point b)))))
           result))
   table)
  (org-roam-tree--sort-file-groups result))))

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
         (push (cons file (sort (nreverse reflinks)
                                (lambda (a b)
                                  (< (org-roam-reflink-point a)
                                     (org-roam-reflink-point b)))))
               result))
       table)
      (org-roam-tree--sort-file-groups result))))

(defun org-roam-tree-unlinked-references (&optional node)

  "Return unlinked references of NODE as a tree using `org-roam-tree-simlink` structs.

Tree format:
((FILENAME
   (SIMLINK SIMLINK ...))
 ...)

NODE defaults to `(org-roam-node-at-point)` if nil."
  (let* ((node (or node (org-roam-node-at-point))))
    (when (and node
               (executable-find "rg")
               (org-roam-node-title node)
               (not (string-match "PCRE2 is not available"
                                  (shell-command-to-string "rg --pcre2-version"))))
      (let* ((titles (cons (org-roam-node-title node)
                           (org-roam-node-aliases node)))
             (temp-file (make-temp-file "org-roam-rg-pattern-"))
             (rg-command (org-roam-unlinked-references--rg-command titles temp-file))
             (file-tree (make-hash-table :test 'equal))) ;; ensure hash table
        (unwind-protect
            (let* ((results (split-string (shell-command-to-string rg-command) "\n"))
                   f row col match body start)
              ;; Build a hash table of filename → list of formatted matches
              (dolist (line results)
                (save-match-data
                  (when (string-match org-roam-unlinked-references-result-re line)
                    (setq f (match-string 1 line)
                          row (string-to-number (match-string 2 line))
                          col (string-to-number (match-string 3 line))
                          match (match-string 4 line)
                          body (propertize (org-roam-fontify-like-in-org-mode (org-roam-unlinked-references-preview-line f row)))
                          start (string-match (regexp-quote match) body))
                    (when (and match
                               (not (file-equal-p (org-roam-node-file node) f))
                               (member (downcase match) (mapcar #'downcase titles)))
                      (when start
                        (put-text-property start (+ start (length match))
                                           'face 'org-link-file body))
                      (setq simlink (make-org-roam-tree-simlink
                                     :title (file-name-nondirectory f)
                                 :file f
                                 :row row
                                 :col col
                                 :match match
                                 :body body))

                      (let* ((matches (gethash f file-tree)))
                        (puthash f (cons simlink matches) file-tree))
                      ))))
              ;; Convert hash table to list of lists, sorted by source-file position
              (let (result)
                (maphash
                 (lambda (filename matches)
                   (push (cons filename (sort (nreverse matches)
                                              (lambda (a b)
                                                (< (org-roam-tree-simlink-row a)
                                                   (org-roam-tree-simlink-row b)))))
                         result))
                 file-tree)
        (org-roam-tree--sort-file-groups result)
                ))
          ;; Clean up temp file
          (delete-file temp-file))))))

(defun org-roam-tree--glob-to-pcre2 (query)
  "Translate a glob-style QUERY string to a PCRE2 pattern string.

Glob wildcards:
  *   matches zero or more word characters (\\w*)
  ?   matches exactly one word character (\\w)
  \\*  literal asterisk
  \\?  literal question mark

Word-boundary anchors (\\b) are added at each end that does not begin/end
with a wildcard.  Literal segments are passed through `regexp-quote' so
that any PCRE2 metacharacters in them are safely escaped."
  (let ((i 0)
        (len (length query))
        (segments '())          ; list of PCRE2 string segments, in reverse
        (lit-buf "")            ; accumulator for current literal run
        (starts-with-wild nil)
        (ends-with-wild nil))
    (while (< i len)
      (let ((ch (aref query i)))
        (cond
         ;; Escape sequence: \* or \? → literal char, anything else → keep backslash+char
         ((and (= ch ?\\) (< (1+ i) len)
               (memq (aref query (1+ i)) '(?* ??)))
          (setq lit-buf (concat lit-buf (string (aref query (1+ i)))))
          (setq i (+ i 2)))
         ;; Glob wildcard *
         ((= ch ?*)
          (unless (string-empty-p lit-buf)
            (push (regexp-quote lit-buf) segments)
            (setq lit-buf ""))
          (when (= i 0) (setq starts-with-wild t))
          (push "\\w*" segments)
          (setq i (1+ i)))
         ;; Glob wildcard ?
         ((= ch ??)
          (unless (string-empty-p lit-buf)
            (push (regexp-quote lit-buf) segments)
            (setq lit-buf ""))
          (when (= i 0) (setq starts-with-wild t))
          (push "\\w" segments)
          (setq i (1+ i)))
         ;; Ordinary character: accumulate into literal buffer
         (t
          (setq lit-buf (concat lit-buf (string ch)))
          (setq i (1+ i))))))
    ;; Flush remaining literal buffer
    (unless (string-empty-p lit-buf)
      (push (regexp-quote lit-buf) segments))
    ;; Determine whether the pattern ends with a wildcard
    (let ((last-seg (car segments)))   ; segments is in reverse, so car = last added
      (when (member last-seg '("\\w*" "\\w"))
        (setq ends-with-wild t)))
    ;; Assemble final pattern with optional \b anchors
    (concat (if starts-with-wild "" "\\b")
            (mapconcat #'identity (nreverse segments) "")
            (if ends-with-wild "" "\\b"))))

(defun org-roam-tree--has-unescaped-glob-p (query)
  "Return non-nil if QUERY contains an unescaped * or ? character."
  (let ((i 0) (len (length query)) found)
    (while (and (< i len) (not found))
      (let ((ch (aref query i)))
        (cond
         ;; Skip escape sequences
         ((and (= ch ?\\) (< (1+ i) len))
          (setq i (+ i 2)))
         ((memq ch '(?* ??))
          (setq found t)
          (setq i (1+ i)))
         (t (setq i (1+ i))))))
    found))

(defun org-roam-tree--has-pcre2-metachar-p (query)
  "Return non-nil if QUERY contains PCRE2 metacharacters other than * and ?.
Detects: . + [ ( { ^ $ and backslash not followed by * ? or b."
  (let ((i 0) (len (length query)) found)
    (while (and (< i len) (not found))
      (let ((ch (aref query i)))
        (cond
         ;; Backslash: check next char
         ((= ch ?\\)
          (if (< (1+ i) len)
              (let ((next (aref query (1+ i))))
                (cond
                 ;; \* and \? are escaped globs — not a metachar
                 ((memq next '(?* ??)) (setq i (+ i 2)))
                 ;; \b is a word boundary — also not a "raw regex" signal
                 ((= next ?b) (setq i (+ i 2)))
                 ;; Any other \X is a regex escape
                 (t (setq found t) (setq i (+ i 2)))))
            ;; Trailing backslash
            (setq found t) (setq i (1+ i))))
         ;; Unambiguous PCRE2 metacharacters (not * or ?)
         ((memq ch '(?. ?+ ?\[ ?\( ?\{ ?^ ?$))
          (setq found t) (setq i (1+ i)))
         (t (setq i (1+ i))))))
    found))

(defun org-roam-tree--has-boolean-keywords-p (query)
  "Return non-nil if QUERY contains whole-word AND, OR, or NOT keywords."
  (or (string-match-p "\\<AND\\>" query)
      (string-match-p "\\<OR\\>"  query)
      (string-match-p "\\<NOT\\>" query)))

(defun org-roam-tree--term-to-pcre2 (term)
  "Convert a single search TERM (possibly with glob wildcards) to PCRE2.
If TERM is double-quoted, strip quotes and treat as an exact literal phrase.
Otherwise apply `org-roam-tree--glob-to-pcre2'."
  (if (and (string-prefix-p "\"" term) (string-suffix-p "\"" term)
           (> (length term) 1))
      ;; Exact quoted phrase
      (format "\\b%s\\b" (regexp-quote (substring term 1 (1- (length term)))))
    (org-roam-tree--glob-to-pcre2 term)))

(defun org-roam-tree--and-term-to-lookaheads (and-term)
  "Convert a single AND-clause AND-TERM (possibly containing NOT) to PCRE2 lookaheads.
Returns a list of lookahead strings.

  \"foo\"         → (\"(?=.*\\\\bfoo\\\\b)\")
  \"NOT foo\"     → (\"(?!.*\\\\bfoo\\\\b)\")
  \"foo NOT bar\" → (\"(?=.*\\\\bfoo\\\\b)\" \"(?!.*\\\\bbar\\\\b)\")"
  (let* ((and-term (string-trim and-term))
         ;; Split on NOT boundaries; first part may be empty if term starts with NOT
         (not-parts (split-string and-term "\\<NOT\\>"))
         (first     (string-trim (car not-parts)))
         (negations (mapcar #'string-trim (cdr not-parts)))
         (result    '()))
    ;; Positive part (may be empty string if clause starts with NOT)
    (unless (string-empty-p first)
      (push (format "(?=.*%s)" (org-roam-tree--term-to-pcre2 first)) result))
    ;; Negative parts
    (dolist (neg negations)
      (unless (string-empty-p neg)
        (push (format "(?!.*%s)" (org-roam-tree--term-to-pcre2 neg)) result)))
    (nreverse result)))

(defun org-roam-tree--boolean-to-pcre2 (query)
  "Convert a boolean search QUERY (AND/OR/NOT keywords) to a PCRE2 pattern.

Supported operators (case-sensitive keywords, evaluated left to right):
  A AND B      →  (?=.*\\bA\\b)(?=.*\\bB\\b).*
  A OR B       →  \\bA\\b|\\bB\\b
  NOT A        →  (?!.*\\bA\\b).*
  A AND NOT B  →  (?=.*\\bA\\b)(?!.*\\bB\\b).*

Precedence: NOT > AND > OR  (standard boolean).
Individual terms are passed through `org-roam-tree--term-to-pcre2' so
glob wildcards work inside boolean expressions."
  ;; OR at top level; within each OR-clause, split on AND; within each AND-
  ;; token, split on NOT.  `--and-term-to-lookaheads' handles the NOT split.
  (let* ((or-clauses (split-string query "\\<OR\\>"))
         (or-parts
          (mapcar
           (lambda (or-clause)
             (let* ((or-clause (string-trim or-clause))
                    (and-tokens (split-string or-clause "\\<AND\\>"))
                    (lookaheads (mapcan #'org-roam-tree--and-term-to-lookaheads
                                        and-tokens)))
               (if (and (= (length lookaheads) 1)
                        (string-prefix-p "(?=" (car lookaheads)))
                   ;; Single positive lookahead: unwrap to a plain match term
                   (org-roam-tree--term-to-pcre2 (string-trim or-clause))
                 ;; Multiple lookaheads, or any negative: chain them then .*
                 (concat (mapconcat #'identity lookaheads "") ".*"))))
           or-clauses)))
    (mapconcat #'identity or-parts "|")))

(defun org-roam-tree--parse-search-query (query)
  "Parse QUERY and return a cons (MODE . PCRE2-PATTERN).

MODE is one of the symbols: literal  glob  boolean  regex

  literal  Plain word, no wildcards, no boolean ops, no regex chars.
           Wrapped with \\b…\\b word boundaries.
  glob     Contains unescaped * or ?, no raw PCRE2 metacharacters.
           Translated via `org-roam-tree--glob-to-pcre2'.
  boolean  Contains AND/OR/NOT keywords.
           Translated via `org-roam-tree--boolean-to-pcre2' (glob also works
           inside boolean terms).
  regex    Contains PCRE2 metacharacters other than * and ?.
           Passed through as-is; caller should omit --only-matching."
  (cond
   ;; 1. Explicit PCRE2 metacharacters → raw regex mode
   ((org-roam-tree--has-pcre2-metachar-p query)
    (cons 'regex query))
   ;; 2. Boolean keywords → boolean mode (glob is handled inside)
   ((org-roam-tree--has-boolean-keywords-p query)
    (cons 'boolean (org-roam-tree--boolean-to-pcre2 query)))
   ;; 3. Glob wildcards → glob mode
   ((org-roam-tree--has-unescaped-glob-p query)
    (cons 'glob (org-roam-tree--glob-to-pcre2 query)))
   ;; 4. Quoted exact phrase → literal phrase match (strip quotes, no \b split)
   ((and (string-prefix-p "\"" query) (string-suffix-p "\"" query)
         (> (length query) 1))
    (cons 'literal (format "\\b%s\\b"
                           (regexp-quote (substring query 1 (1- (length query)))))))
   ;; 5. Plain literal → exact word-boundary match
   (t
    (cons 'literal (format "\\b%s\\b" (regexp-quote query))))))

(defun org-roam-tree--search-string-rg-command (search-string temp-file &optional regex-p)
  "Return a ripgrep command that searches for SEARCH-STRING across the org-roam directory.
Writes the PCRE2 pattern to TEMP-FILE to avoid shell-escaping issues.
SEARCH-STRING should already be a PCRE2 pattern (as returned by
`org-roam-tree--parse-search-query').
When REGEX-P is non-nil, omit --only-matching (needed for lookahead-based patterns)."
  (with-temp-file temp-file
    (insert search-string))
  (concat "rg --follow"
          (unless regex-p " --only-matching")
          " --vimgrep --pcre2 --ignore-case "
          (mapconcat (lambda (glob) (concat "--glob " glob))
                     (org-roam--list-files-search-globs org-roam-file-extensions)
                     " ")
          " --file " (shell-quote-argument temp-file) " "
          (shell-quote-argument (expand-file-name org-roam-directory))))

(defun org-roam-tree-search-string (&optional search-string)
  "Return matches for SEARCH-STRING across the roam directory as a simlink tree.

When called interactively (or with SEARCH-STRING nil) prompts the user
for the string to search.

Tree format:
  ((FILENAME . (SIMLINK SIMLINK ...)) ...)

Unlike `org-roam-tree-unlinked-references', this searches for an
arbitrary query rather than the current node's title or aliases,
and does not exclude any file.

The query is interpreted according to its content (see
`org-roam-tree--parse-search-query' for details):

  Literal   Plain words → exact word-boundary match.
  Glob      Words with * or ? wildcards → e.g. begin* matches beginning.
  Boolean   AND / OR / NOT keywords → e.g. \"emacs AND org* NOT export\".
  Regex     Raw PCRE2 when the query contains metacharacters like . + [ ( etc."
  (let ((search-string (or search-string
                           (read-string "Search org-roam for: "))))
    (when (and (not (string-empty-p search-string))
               (executable-find "rg")
               (not (string-match "PCRE2 is not available"
                                  (shell-command-to-string "rg --pcre2-version"))))
      (let* ((parsed    (org-roam-tree--parse-search-query search-string))
             (pcre2     (cdr parsed))
             (regex-p   (memq (car parsed) '(regex boolean)))
             (temp-file (make-temp-file "org-roam-rg-pattern-"))
             (rg-command (org-roam-tree--search-string-rg-command pcre2 temp-file regex-p))
             (file-tree (make-hash-table :test 'equal)))
        (unwind-protect
            (let* ((results (split-string (shell-command-to-string rg-command) "\n"))
                   f row col match body start)
              (dolist (line results)
                (save-match-data
                  (when (string-match org-roam-unlinked-references-result-re line)
                    (setq f     (match-string 1 line)
                          row   (string-to-number (match-string 2 line))
                          col   (string-to-number (match-string 3 line))
                          match (match-string 4 line)
                          body  (propertize
                                 (org-roam-fontify-like-in-org-mode
                                  (org-roam-unlinked-references-preview-line f row)))
                          start (and match (string-match (regexp-quote match) body)))
                    (when match
                      (when start
                        (put-text-property start (+ start (length match))
                                           'face 'org-link-file body))
                      (let ((simlink (make-org-roam-tree-simlink
                                      :title (file-name-nondirectory f)
                                      :file f
                                      :row row
                                      :col col
                                      :match match
                                      :body body)))
                        (puthash f (cons simlink (gethash f file-tree)) file-tree))))))
              (let (result)
                (maphash (lambda (filename matches)
                           (push (cons filename (sort (nreverse matches)
                                                      (lambda (a b)
                                                        (< (org-roam-tree-simlink-row a)
                                                           (org-roam-tree-simlink-row b)))))
                                 result))
                         file-tree)
                (org-roam-tree--sort-file-groups result)))
          (delete-file temp-file))))))

(cl-defun org-roam-tree-simlink-insert-section (simlink)
  "Insert a section for SIMLINK in the org-roam tree buffer.
This mirrors `org-roam-node-insert-section`, but works for simulated links."
  (let ((title (org-roam-tree-simlink-title simlink))
        (file  (org-roam-tree-simlink-file simlink))
        (row   (org-roam-tree-simlink-row simlink))
        (col   (org-roam-tree-simlink-col simlink))
        (point (org-roam-tree-simlink-point simlink))
        (body  (org-roam-tree-simlink-body simlink))
        (props (org-roam-tree-simlink-properties simlink)))
    ;; Parent section for the file/title
    (magit-insert-section section (org-roam-node-section simlink)
      (oset section keymap 'org-roam-tree-simlink-map)
      (insert (propertize title 'font-lock-face 'org-roam-title)
              (when (and row col) (format " (%d:%d)" row col))))
    ;; Child section for the content
    (magit-insert-heading)
    (magit-insert-section section (org-roam-grep-section simlink)
      (insert body "\n")
      (oset section file file)
      ;(when point (oset section point point))
      (oset section row row)
      (oset section col col)
      ;(oset section properties props)
      (oset section keymap 'org-roam-tree-simlink-map)
      (insert ?\n))))

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
              (push (cons file (sort (nreverse bls)
                                     (lambda (a b)
                                       (< (org-roam-backlink-point a)
                                          (org-roam-backlink-point b)))))
                    files))
            file-table)
           (let ((crosslink-node (org-roam-node-from-id crosslink-id)))
             (push (cons (if crosslink-node
                             (org-roam-node-title crosslink-node)
                           crosslink-id) ;;; occasional nil errors unless I do this
                         (nreverse files))
                   result))))
       table)
(sort result
              (lambda (a b)
                (> (length (cdr a))
                   (length (cdr b))))))))



;;;;;;;;;;;;;;;;;;;; Linked/unlinked reference conversion
;; Helpers and keymap to convert unlinked references to proper backlinks
;;

(defun org-roam-tree-simlink-visit ()
  "Visit the file location of the simlink section at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (simlink (oref section value)))
    (find-file (org-roam-tree-simlink-file simlink))
    (goto-char (point-min))
    (forward-line (1- (org-roam-tree-simlink-row simlink)))
    (move-to-column (max 0 (1- (org-roam-tree-simlink-col simlink))))))


(defun org-roam-tree-convert-unlinked-reference (&optional simlink)
  "Convert SIMLINK (or the one at point) into a proper ID link back to
the node currently shown in the org-roam buffer."
  (interactive)
  (let* ((simlink (or simlink
                       (let ((section (magit-current-section)))
                         (and section (org-roam-tree-simlink-p (oref section value))
                              (oref section value)))))
         (node org-roam-buffer-current-node))
    (unless (org-roam-tree-simlink-p simlink)
      (user-error "No unlinked reference at point"))
    (unless node
      (user-error "No org-roam node found for this buffer"))
    (org-roam-tree--convert-simlink-to-backlink simlink node)))

(defun org-roam-tree--convert-simlink-to-backlink (simlink node)
  "Rewrite the text SIMLINK matched in its source file as an ID link
to NODE, then refresh the org-roam db and buffer."
  (let* ((mark-buf (current-buffer))
         (mark-pos (point))
         (file    (org-roam-tree-simlink-file simlink))
         (row     (org-roam-tree-simlink-row simlink))
         (col     (org-roam-tree-simlink-col simlink))
         (match   (org-roam-tree-simlink-match simlink))
         (node-id (org-roam-node-id node))
         (buf     (find-file-noselect file)))
    (unless (and match (> (length match) 0))
      (user-error "No matched text stored for this reference; re-run the unlinked references search"))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- row))
        (let ((p1 (save-excursion (move-to-column (max 0 (1- col))) (point)))
              (p2 (save-excursion (move-to-column col) (point))))
          (goto-char
           (cond
            ((save-excursion (goto-char p1) (looking-at (regexp-quote match))) p1)
            ((save-excursion (goto-char p2) (looking-at (regexp-quote match))) p2)
            (t (beginning-of-line)
               (unless (re-search-forward (regexp-quote match) (line-end-position) t)
                 (user-error "Couldn't find %S on line %d of %s — the file may have changed"
                             match row (file-name-nondirectory file)))
               (match-beginning 0))))
          (delete-region (point) (+ (point) (length match)))
          (insert (format "[[id:%s][%s]]" node-id match))
          (org-id-get-create)
          ))
      (save-buffer))
    (org-roam-db-update-file file)
    (org-roam-tree--refresh-or-mark mark-buf mark-pos 'org-roam-tree-added-face)
    (message "Linked %S to %S" match (org-roam-node-title node))))


;; Backlink removal: Convert a real ID-linked backlink back into plain text, in place

(defun org-roam-tree-remove-backlink (&optional backlink)
  "Remove the ID link for BACKLINK (or the one at point) in its
source file, replacing it with its plain display text so the
connection is no longer a link."
  (interactive)
  (let* ((backlink (or backlink
                        (let ((value (org-roam-tree--leaf-value-at-point)))
                          (and (org-roam-backlink-p value) value)))))
    (unless backlink
      (user-error "No backlink at point"))
    (let* ((source-node (org-roam-backlink-source-node backlink))
           (target-node (org-roam-backlink-target-node backlink))
           (source-title (and source-node (org-roam-node-title source-node)))
           (target-title (or (and target-node (org-roam-node-title target-node))
                              (and org-roam-buffer-current-node
                                   (org-roam-node-title org-roam-buffer-current-node))))
           (link-text (org-roam-tree--backlink-link-text backlink)))
      (when (yes-or-no-p (format "Remove backlink %S from %S to node %S? "
                                  (or link-text "?") source-title target-title))
        (org-roam-tree--remove-backlink-link backlink)))))

(defun org-roam-tree--backlink-link-text (backlink)
  "Return the displayed text of BACKLINK's link in its source file,
without modifying anything. Returns nil if the link can't be found
at the recorded position."
  (let* ((source-node (org-roam-backlink-source-node backlink))
         (file (org-roam-node-file source-node))
         (pos  (org-roam-backlink-point backlink))
         (buf  (and pos (find-file-noselect file))))
    (when buf
      (with-current-buffer buf
        (save-excursion
          (goto-char pos)
          (let ((link (org-element-context)))
            (when (eq (org-element-type link) 'link)
              (let ((cbeg (org-element-property :contents-begin link))
                    (cend (org-element-property :contents-end link)))
                (if (and cbeg cend)
                    (buffer-substring-no-properties cbeg cend)
                  (org-element-property :raw-link link))))))))))

(defun org-roam-tree--remove-backlink-link (backlink)
  "Replace the ID link represented by BACKLINK in its source file
with its plain display text, then refresh the org-roam db and
buffer."
  (let* ((mark-buf (current-buffer))
         (mark-pos (point))
         (source-node (org-roam-backlink-source-node backlink))
         (file (org-roam-node-file source-node))
         (pos  (org-roam-backlink-point backlink))
         (buf  (find-file-noselect file))
         replacement)
    (unless pos
      (user-error "No position recorded for this backlink"))
    (with-current-buffer buf
      (save-excursion
        (goto-char pos)
        (let ((link (org-element-context)))
          (unless (eq (org-element-type link) 'link)
            (user-error "No link found at the recorded position — the file may have changed"))
          (let* ((begin (org-element-property :begin link))
                 (end   (- (org-element-property :end link)
                           (or (org-element-property :post-blank link) 0)))
                 (cbeg  (org-element-property :contents-begin link))
                 (cend  (org-element-property :contents-end link)))
            (setq replacement (if (and cbeg cend)
                                   (buffer-substring-no-properties cbeg cend)
                                 (org-element-property :raw-link link)))
            (goto-char begin)
            (delete-region begin end)
            (insert replacement))))
      (save-buffer))
    (org-roam-db-update-file file)
    (org-roam-tree--refresh-or-mark mark-buf mark-pos 'org-roam-tree-removed-face)
    (message "Removed link, kept text: %S" replacement)))


(defun org-roam-tree--mark-leaf-at (buffer pos face)
  "Apply FACE to the tree node's displayed text at POS in BUFFER, as
a lightweight visual marker in lieu of a full refresh."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((bounds (org-roam-tree--node-text-bounds pos))
             (start (car bounds))
             (end   (cdr bounds))
             (inhibit-read-only t))
        (add-face-text-property start end face)))))

(defun org-roam-tree--refresh-or-mark (buffer pos face)
  "Refresh the org-roam buffer if `org-roam-tree-auto-refresh-buffer'
is non-nil; otherwise mark the tree entry at POS in BUFFER with FACE
instead."
  (if org-roam-tree-auto-refresh-buffer
      (when (get-buffer-window org-roam-buffer)
        (org-roam-buffer-refresh))
    (org-roam-tree--mark-leaf-at buffer pos face)))


;;;;;;;;;;;;;;;;;;;; Reference copying
;; Functions to quickly copy backlink content to another open buffer
;;
(defun org-roam-tree--node-text-bounds (pos)
  "Return (START . END) of the tree node at POS, found via metadata
text properties rather than magit-section's (possibly stale) start/end."
  (let (start end)
    (save-excursion
      (goto-char pos)
      (beginning-of-line)
      (while (and (> (point) (point-min))
                  (not (get-text-property (point) org-roam-tree--meta-depth)))
        (forward-line -1))
      (setq start (point))
      (forward-line 1)
      (while (and (not (eobp))
                  (not (get-text-property (point) org-roam-tree--meta-depth)))
        (forward-line 1))
      (setq end (point)))
    (cons start end)))

(defun org-roam-tree--section-preview-text (section)
  "Return SECTION's displayed body text, from its heading's end to
its end, with tree-prefix decoration stripped."
  (unless (cl-typep (oref section value)
                     '(or org-roam-backlink org-roam-reflink org-roam-tree-simlink))
    (user-error "Point is not on a quotable section (got %s)"
                (type-of (oref section value))))
  ;; section start/end are plain integers captured at insertion time, and
  ;; can go stale once org-roam-tree--prefix-node-content starts inserting
  ;; text (prefixes, hard newlines) into the buffer during jit-lock passes.
  ;; Use the metadata text properties instead, which move correctly with
  ;; the buffer, to find the node's real current boundaries.
  (let* ((bounds (org-roam-tree--node-text-bounds (oref section start)))
         (start (car bounds))
         (end   (cdr bounds))
         (chunks nil))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((line-start (point))
               (line-end (min end (line-end-position))))
          (unless (get-text-property line-start org-roam-tree--meta-is-prefix-string)
            (push (buffer-substring-no-properties line-start line-end) chunks))
          ;; if the line *starts* with a prefix, find where the prefix ends
          ;; and grab the remainder instead of dropping the whole line
          (when (get-text-property line-start org-roam-tree--meta-is-prefix-string)
            (let ((prefix-end
                   (next-single-property-change
                    line-start org-roam-tree--meta-is-prefix-string nil line-end)))
              (when (and prefix-end (< prefix-end line-end))
                (push (buffer-substring-no-properties prefix-end line-end) chunks))))
          (goto-char (min end (1+ line-end))))))
    (string-trim (mapconcat #'identity (nreverse chunks) ""))))

(defun org-roam-tree--simlink-source-link (simlink)
  "Return (ID-OR-NIL . LINK-TEXT) describing where SIMLINK points,
preferring an ID link to the enclosing org-roam node if one can be
found, falling back to a `file:' link at the matched line."
  (let* ((file (org-roam-tree-simlink-file simlink))
         (row  (org-roam-tree-simlink-row simlink)))
    (if (string-match-p "\\.org\\'" file)
        (let* ((buf (find-file-noselect file))
               (node (with-current-buffer buf
                       (save-excursion
                         (goto-char (point-min))
                         (forward-line (1- row))
                         (org-roam-node-at-point)))))
          (if node
              (cons (format "[[id:%s][%s]]"
                            (org-roam-node-id node)
                            (org-roam-node-title node))
                    nil)
            (cons (format "[[file:%s::%d][%s]]" file row
                          (file-name-nondirectory file))
                  nil)))
      (cons (format "[[file:%s::%d][%s]]" file row
                    (file-name-nondirectory file))
            nil))))

(defun org-roam-tree--leaf-value-at-point ()
  "Return the backlink/reflink/simlink at point, preferring the
`org-roam-tree-leaf-value' text property set at insertion time, and
falling back to walking up section parents for it."
  (or (get-text-property (point) 'org-roam-tree-leaf-value)
      (let ((section (magit-current-section)))
        (while (and section
                    (not (cl-typep (oref section value)
                                   '(or org-roam-backlink org-roam-reflink org-roam-tree-simlink))))
          (setq section (oref section parent)))
        (and section (oref section value)))))

(defun org-roam-tree--value-source-link (value)
  "Return an org link string describing the source of VALUE (a
backlink, reflink, or simlink)."
  (cl-typecase value
    (org-roam-backlink
     (let ((node (org-roam-backlink-source-node value)))
       (format "[[id:%s][%s]]" (org-roam-node-id node) (org-roam-node-title node))))
    (org-roam-reflink
     (let ((node (org-roam-reflink-source-node value)))
       (format "[[id:%s][%s]]" (org-roam-node-id node) (org-roam-node-title node))))
    (org-roam-tree-simlink
     (car (org-roam-tree--simlink-source-link value)))
    (t (user-error "No source link available for this position"))))

(defun org-roam-tree--preview-text-at-point ()
  "Return the displayed body text of the tree node at point, with
tree-prefix decoration stripped."
  (let* ((bounds (org-roam-tree--node-text-bounds (point)))
         (start (car bounds))
         (end   (cdr bounds))
         (chunks nil))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((line-start (point))
               (line-end (min end (line-end-position))))
          (unless (get-text-property line-start org-roam-tree--meta-is-prefix-string)
            (push (buffer-substring-no-properties line-start line-end) chunks))
          (when (get-text-property line-start org-roam-tree--meta-is-prefix-string)
            (let ((prefix-end (next-single-property-change
                                line-start org-roam-tree--meta-is-prefix-string nil line-end)))
              (when (and prefix-end (< prefix-end line-end))
                (push (buffer-substring-no-properties prefix-end line-end) chunks))))
          (goto-char (min end (1+ line-end))))))
    (string-trim (mapconcat #'identity (nreverse chunks) ""))))

(defun org-roam-tree--build-quote-block ()
  "Build the #+begin_quote block text for the leaf node at point."
  (let* ((value (org-roam-tree--leaf-value-at-point)))
    (unless value
      (user-error "No quotable backlink/reflink/simlink at point"))
    (format "\n#+begin_quote\n%s %s\n%s\n#+end_quote\n\n"
            org-roam-tree-quote-source-prefix
            (org-roam-tree--value-source-link value)
            (org-roam-tree--preview-text-at-point))))

(defun org-roam-tree--insert-quote-string (quote-text)
  "Insert the already-built QUOTE-TEXT at point, skipping past any
enclosing #+begin_quote...#+end_quote first."
  (when (org-in-block-p '("quote"))
    (re-search-forward "^[ \t]*#\\+end_quote[ \t]*$" nil t)
    (forward-line 1)
    (beginning-of-line))
  (insert quote-text))


(defun org-roam-tree--insert-quote-block (section)
  "Insert SECTION's quote block at point, skipping past any
enclosing #+begin_quote...#+end_quote first."
  (when (org-in-block-p '("quote"))
    (re-search-forward "^[ \t]*#\\+end_quote[ \t]*$" nil t)
    (forward-line 1)
    (beginning-of-line))
  (insert (org-roam-tree--build-quote-block section)))


;;;;;;;; The functions to call from context menu
;; copy to org-roam-buffer's own source node buffer
(defun org-roam-tree-quote-to-node ()
  "Copy the quote at point into the buffer visiting the current
org-roam node, at that buffer's own window point. Errors if that
buffer has no visible window."
  (interactive)
  (let* ((node org-roam-buffer-current-node)
         (file (and node (org-roam-node-file node)))
         (buf (and file (get-file-buffer file)))
         (win (and buf (get-buffer-window buf))))
    (unless win
      (user-error "Node buffer is not visible in any window"))
    ;; build the quote text now, while *org-roam* is still current --
    ;; extraction relies on buffer-local text properties/positions that
    ;; are only meaningful in this buffer
    (let ((quote-text (org-roam-tree--build-quote-block)))
      (with-selected-window win
        (org-roam-tree--insert-quote-string quote-text)))))

;; copy to any arbitrary buffer

(defun org-roam-tree--quote-target-candidates ()
  "Return candidate buffer names for quote-copy targets, most
recently used first, filtered by
`org-roam-tree-quote-target-predicate'."
  (let* ((valid (cl-remove-if-not org-roam-tree-quote-target-predicate (buffer-list)))
         (names (mapcar #'buffer-name valid)))
    (append
     (cl-remove-if-not (lambda (n) (member n names))
                        org-roam-tree--quote-target-history)
     (cl-set-difference names org-roam-tree--quote-target-history :test #'equal))))

(defun org-roam-tree--read-target-buffer ()
  "Prompt for a buffer to copy a quote into. Uses consult's preview
UI if available, else plain `completing-read'. Returns a buffer."
  (let* ((candidates (org-roam-tree--quote-target-candidates))
         (choice
          (if (and (fboundp 'consult--read)
                    (fboundp 'consult--buffer-state))
              (consult--read
               candidates
               :prompt "Copy quote to buffer: "
               :require-match t
               :sort nil
               :category 'buffer
               :state (consult--buffer-state))
            (completing-read "Copy quote to buffer: " candidates nil t))))
    (setq org-roam-tree--quote-target-history
          (cons choice (remove choice org-roam-tree--quote-target-history)))
    (get-buffer choice)))

(defun org-roam-tree-quote-to-buffer ()
  "Copy the quote at point into a buffer selected via completing-read,
at that buffer's own point. Does not change window focus."
  (interactive)
    ;; same ordering requirement as org-roam-tree-quote-to-node
    (let ((quote-text (org-roam-tree--build-quote-block))
          (target (org-roam-tree--read-target-buffer)))
      (with-current-buffer target
        (org-roam-tree--insert-quote-string quote-text))))

(defun org-roam-tree-quote-to-kill-ring ()
  "Copy the quote at point into kill-ring."
  (interactive)
    ;; same ordering requirement as org-roam-tree-quote-to-node
    (let ((quote-text (org-roam-tree--build-quote-block)))
        (kill-new quote-text)))

(defvar org-roam-tree-backlink-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-roam-node-map)
    (define-key map (kbd "r") #'org-roam-tree-remove-backlink)
    (define-key map (kbd "q") #'org-roam-tree-quote-to-node)
    (define-key map (kbd "Q") #'org-roam-tree-quote-to-buffer)
    (define-key map (kbd "y") #'org-roam-tree-quote-to-kill-ring)
    (define-key map [mouse-3] #'org-roam-tree--quote-popup-menu)
    map))

(defvar org-roam-tree-simlink-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-roam-tree-simlink-visit)
    (define-key map [mouse-1]   #'org-roam-tree-simlink-visit)
    (define-key map [mouse-3]   #'org-roam-tree-simlink-context-menu)
    (define-key map (kbd "c")   #'org-roam-tree-convert-unlinked-reference)
    (define-key map (kbd "q") #'org-roam-tree-quote-to-node)
    (define-key map (kbd "Q") #'org-roam-tree-quote-to-buffer)
    (define-key map (kbd "y") #'org-roam-tree-quote-to-kill-ring)
    map)
  "Keymap active on simlink sections in the unlinked-references tree.")


(defun org-roam-tree-simlink-context-menu (event)
  "Right-click context menu for the unlinked-reference section at EVENT."
  (interactive "e")
  (let ((win (posn-window (event-start event))))
    (with-selected-window win
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (when-let* ((section (magit-current-section))
                    (simlink (oref section value)))
          (popup-menu
           (list "Unlinked reference"
                 (vector "Convert to backlink"
                         (list #'org-roam-tree-convert-unlinked-reference
                               (list 'quote simlink)))
                 (vector "Quote to node buffer"
                         (list #'org-roam-tree-quote-to-node))
                 (vector "Quote to buffer..."
                         (list #'org-roam-tree-quote-to-buffer))
                 (vector "Quote to kill-ring..."
                         (list #'org-roam-tree-quote-to-kill-ring))
                         )))))))

(defun org-roam-tree--quote-popup-menu (event)
  (interactive "e")
  (let ((win (posn-window (event-start event))))
    (with-selected-window win
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let* ((value (org-roam-tree--leaf-value-at-point))
               (items (list ["Quote to node buffer" org-roam-tree-quote-to-node]
                            ["Quote to buffer..."   org-roam-tree-quote-to-buffer]
                            ["Quote to kill-ring..."   org-roam-tree-quote-to-kill-ring]
                            )))
          (when (org-roam-backlink-p value)
            (push ["Remove backlink" org-roam-tree-remove-backlink] items))
          (popup-menu (cons "Actions" (nreverse items))))))))



;;;;;;;;;;;;;;;;;;;; Folding logic
;; What is shown, what is collapsed; track state when navigating between
;; nodes.
;;

(defun org-roam-tree--apply-folded-state ()
  "Walk the Org-roam tree buffer and fold sections based on stored metadata.
Must run with the org-roam window selected so that magit's section
visibility machinery and `vertical-motion' use the correct window geometry."
  (when-let ((win (get-buffer-window org-roam-buffer)))
    (with-selected-window win
      (save-excursion
        (goto-char (point-min))
        (vertical-motion 1)
        (while (and (not (eobp))
                    (not (eq (magit-current-section) magit-root-section)))
          (when-let ((sec (magit-current-section)))
            (magit-section-show-children sec))
          (when (get-text-property (point) org-roam-tree--meta-depth)
            (let* ((meta  (org-roam-tree--get-node-metadata (point)))
                   (node-id (org-roam-node-id org-roam-buffer-current-node))
                   (path  (plist-get meta :path))
                   (depth (plist-get meta :depth))
                   (visible (org-roam-tree--node-visible-state node-id path)))
              (when (and (if (booleanp visible)
                             visible
                           (> depth visible))
                         (not (magit-section-hidden (magit-current-section))))
                (forward-char (* depth 3)) ; ensure point is inside the section
                (magit-section-hide (magit-current-section)))))
          (magit-section-forward))
        (redisplay t)))))

(defun org-roam-tree--track-toggle (&rest _args)
  "Save fold state for the section just toggled."
  (when-let ((section (magit-current-section)))
    (let*
        ((inhibit-read-only t)
         (node org-roam-buffer-current-node)
              (start (oref section start))
              (path (get-text-property start org-roam-tree--meta-path))
              (hidden (not (org-roam-tree--node-visible-state (org-roam-node-id node) path))))

    ;; Store state: visible = not hidden
      (unless hidden
        (with-org-roam-tree-layout
         (goto-char start)
         
         (org-roam-tree--prefix-node-content )
         )
        )
      (org-roam-tree--set-node-visible-state node path hidden))))

(advice-add 'magit-section-toggle :after #'org-roam-tree--track-toggle)

(defun org-roam-tree--refontify-toggled-section (&rest _)
  "Refontify the currently toggled Magit section for org-roam-tree."
  (when (org-roam-tree--active-p) ;; your existing buffer check
    (let ((section (magit-current-section)))
      (when section
        (jit-lock-refontify
         (oref section start)
         (oref section end))))
    (redisplay)
    ))

(advice-add 'magit-section-toggle :after
            #'org-roam-tree--refontify-toggled-section)


;;;;;;;;;;;;;;;;;;;; MENU BUTTONs
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


;;;;;; pin the org-roam buffer to its current view
(defcustom org-roam-tree-follow-point t
  "Whether org-roam-tree follows point."
  :type 'boolean
  :group 'org-roam-tree)

(defun org-roam-tree-toggle-follow-point ()
  (interactive)
  (setq org-roam-tree-follow-point
        (not org-roam-tree-follow-point))
  (message "Org-roam follow point %s"
           (if org-roam-tree-follow-point "enabled" "disabled"))

  (if org-roam-tree-follow-point
      (setq org-roam-tree--follow-icon "👁")
    (setq org-roam-tree--follow-icon "🖈"))
  (org-roam-tree--update-buttons)
  (org-roam-tree--add-header-buttons)
  )

(if org-roam-tree-follow-point
      (setq org-roam-tree--follow-icon "👁")
    (setq org-roam-tree--follow-icon "🖈"))

(defun org-roam-tree--redisplay-h-advice (orig-fun &rest args)
  (when org-roam-tree-follow-point
    (apply orig-fun args)))

(advice-add 'org-roam-buffer--redisplay-h
            :around
            #'org-roam-tree--redisplay-h-advice)


(defun org-roam-tree--update-buttons()
  (setq org-roam-tree--header-buttons
        (list
         (org-roam-tree--make-button org-roam-tree--follow-icon #'org-roam-tree-toggle-follow-point
                                     :help "Toggle follow")

                                        ; hamburger menu
         (org-roam-tree--make-button "" #'org-roam-tree--header-menu
                                        ;:help "Menu"
                                     ))))
(org-roam-tree--update-buttons)

(defun org-roam-tree--helper-ensure-buffer-focus (event fn &rest args)
  "Ensure the clicked window is selected, then call FN with ARGS."
  (interactive "e")
  (let ((win (posn-window (event-start event))))
    (select-window win))
  (apply fn args))

(setq org-roam-tree--roam-sections-cookie org-roam-mode-sections)

(defun org-roam-tree--search-string-from-menu ()
  "Prompt for a search string, then switch the roam buffer to the search section.
Called from the header menu. Prompts are issued *before* any rendering so
that `get-buffer-window' is reliable during the subsequent buffer refresh."
  (interactive)
  (let ((query (read-string "Search org-roam for: ")))
    (unless (string-empty-p query)
      (org-roam-tree--change-sections
       (list (list #'org-roam-tree-search-string-section
                   :search-string query))))))

(defun org-roam-tree--header-menu ()
  (popup-menu
   `("menu"
     ["Default section" (org-roam-tree--change-sections org-roam-tree--roam-sections-cookie)]
     ["Backlinks tree"            (org-roam-tree--change-sections '(org-roam-tree-backlinks-section))]
     ["Reflinks tree"             (org-roam-tree--change-sections '(org-roam-tree-reflinks-section))]
     ["Unlinked References tree"  (org-roam-tree--change-sections '(org-roam-tree-unlinked-references-section))]
     ["Crosslinks tree"           (org-roam-tree--change-sections '(org-roam-tree-crosslinks-section))]
     ["Search string..."          (org-roam-tree--search-string-from-menu)]
     "---"
     ("Sort"
      ["Hits (most matches first)"      (org-roam-tree--set-sort-mode 'hits)
       :style radio :selected ,(eq org-roam-tree-sort-mode 'hits)]
      ["Recency (most recent first)"    (org-roam-tree--set-sort-mode 'recency)
       :style radio :selected ,(eq org-roam-tree-sort-mode 'recency)]
      ["Name (alphabetical)"            (org-roam-tree--set-sort-mode 'name)
       :style radio :selected ,(eq org-roam-tree-sort-mode 'name)]))))

(defun org-roam-tree--change-sections (sections)
  "Change the sections displayed in org-roam buffer to sections and
reload."
  (message "Changing roam buffer sections...")
  (setq org-roam-mode-sections sections)
  (org-roam-buffer-refresh))

(defun org-roam-tree--active-search-query ()
  "Return the search query string if a search-string section is currently
active in `org-roam-mode-sections', otherwise nil."
  (cl-some (lambda (s)
              (when (and (consp s)
                         (eq (car s) 'org-roam-tree-search-string-section))
                (plist-get (cdr s) :search-string)))
            org-roam-mode-sections))

(defun org-roam-tree--add-header-buttons ()
  (when (or org-roam-buffer-current-node
            (org-roam-tree--active-search-query))
    (let* ((query (org-roam-tree--active-search-query))
           (title (if query
                      (propertize (format "Search: %s" query) 'face 'bold)
                    (propertize (org-roam-node-title org-roam-buffer-current-node)
                                'face 'bold)))
           (btn-list org-roam-tree--header-buttons)
           (btn-width
            (apply #'+
                   (mapcar (lambda (btn)
                             (+ 2 (string-width btn)))
                           btn-list))))
      (setq header-line-format
            `(,title
              (:eval (propertize
                      " "
                      'display '((space :align-to (- right ,btn-width)))))
              ,@(cl-mapcan (lambda (btn) (list btn " "))
                           btn-list))))))

(add-hook 'org-roam-buffer-postrender-functions
          #'org-roam-tree--add-header-buttons)

(add-hook 'org-roam-buffer-postrender-functions
          #'org-roam-tree--apply-folded-state)

(defun org-roam-tree--redisplay-after-render ()
  "Force redisplay of org-roam buffer after node navigation."
  (when (org-roam-tree--active-p)
    (redisplay)))

(add-hook 'org-roam-buffer-postrender-functions
          #'org-roam-tree--redisplay-after-render)

(provide 'org-roam-tree)
;;; org-roam-tree.el ends here
