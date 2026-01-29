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
;;(setq! org-roam-mode-sections '(org-roam-tree-backlinks-section org-roam-tree-reflinks-section))
;;
;; Add this section with the others in the org-roam buffer:
;;(add-to-list 'org-roam-mode-sections
;;             #'org-roam-tree-backlinks-section t)
;;
;; DO NOT DO THIS YET:
;;(setq! org-roam-mode-sections '(org-roam-tree-reflinks-section org-roam-tree-backlinks-section))
;;having two trees in the same roam buffer is currently broken.
;;

(require 'org-roam)

(defgroup org-roam-tree nil
  "Tree-style display extensions for Org-roam."
  :group 'org-roam)

(defcustom org-roam-tree-default-visible t
  "Whether to collapse all file-level branches after rendering the Org-roam tree."
  :type 'boolean
  :group 'org-roam-tree)


(defvar org-roam-tree-visible-state (make-hash-table :test 'equal)
  "Stores fold states for files per node.")

(defun org-roam-tree--file-visible-state (node file)
  "Return t if FILE under NODE should be folded, either because it has been
toggled by user or because of -default-visibility."
  (gethash (cons node file) org-roam-tree-visible-state org-roam-tree-default-visible)
      )

(defun org-roam-tree--set-file-visible-state (node file hidden)
  "Store fold state for FILE under NODE."
  (puthash (cons node file) hidden org-roam-tree-visible-state))


(cl-defun org-roam-tree-backlinks-section (node &key (section-heading "Backlinks Tree:"))
  "A tree-style backlinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-backlinks :section-id 'backlinks-tree))

(cl-defun org-roam-tree-reflinks-section (node &key (section-heading "Reflinks Tree:"))
  "A tree-style backlinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-reflinks :section-id 'reflinks-tree))

(cl-defun org-roam-tree-reflinks-section (node &key (section-heading "Reflinks Tree:"))
  "A tree-style backlinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-reflinks))





;;;;;;;;;;;;;;;;;;;; TREE DISPLAY METADATA
;; On building the tree, we store metadata including depth and tree branch parts
;; (is-last for each level) as text properties, but do not immediately decorate,
;; as it is quite an expensive operation. We start the tree folded, and decorate
;; on expand to amortize the display cost.
 
;;;;;; Metadata storage and retrieval functions:

(defconst org-roam-tree--meta-depth 'org-roam-tree-depth)
(defconst org-roam-tree--meta-is-last 'org-roam-tree-is-last)
(defconst org-roam-tree--meta-prefixed 'org-roam-tree-prefixed)

(defun org-roam-tree--store-node-metadata (pos depth is-last-vec)
  "Store tree metadata at POS, the start of a node heading."
  (add-text-properties
   pos (1+ pos)
   (list
    org-roam-tree--meta-depth depth
    org-roam-tree--meta-is-last (copy-sequence is-last-vec)
    org-roam-tree--meta-prefixed nil)))

(defun org-roam-tree--get-node-metadata (pos)
  "Return node tree metadata plist stored at POS."
  (list
   :depth    (get-text-property pos org-roam-tree--meta-depth)
   :is-last  (get-text-property pos org-roam-tree--meta-is-last)
   :prefixed (get-text-property pos org-roam-tree--meta-prefixed)))

(defun org-roam-tree--message-node-metadata (pos)
  "Message node metadata at POS for debugging."
  (let ((meta (org-roam-tree--get-node-metadata pos)))
    (message "[tree-node] pos=%d depth=%S is-last=%S prefixed=%S"
             pos
             (plist-get meta :depth)
             (plist-get meta :is-last)
             (plist-get meta :prefixed))))



;;;;;;;;;;;;;;;;;;;; TREE DISPLAY LOGIC
(cl-defun org-roam-tree-section
    (node &key
          (section-heading "Tree Section:")
          (data-getter #'org-roam-tree-backlinks)
          (section-id 'org-roam-tree-section))

  (with-org-roam-tree-layout
   (when-let ((tree (funcall data-getter node)))
     (magit-insert-section section-id
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

(defun org-roam-tree--render-node (node depth is-last-vec)
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
            'org-roam-tree-node))))

    (magit-insert-section section-id value
      ;; Insert this node’s content
      (org-roam-tree--insert-leaf value children)

      ;; store tree metadata at node start
      (org-roam-tree--store-node-metadata start depth is-last-vec)
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
                    is-last-vec)))))
    
    ))

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
  "Save fold state for the file section after toggling."
  (let* ((section (magit-current-section))
      (node org-roam-current-node)
             (file (string-remove-prefix "org-roam-tree-file-" (oref section value)))
             (hidden (oref section hidden)))  ;; true if folded
        (org-roam-tree--set-file-visible-state (org-roam-node-id node) file (not hidden))))

(advice-add 'magit-section-toggle :after #'org-roam-tree--track-toggle)

(defun org-roam-tree--prefix-node-content (depth)
  "Insert tree prefixes for a node's rendered content.

Assumes point is at the beginning of the node. Uses metadata
stored at point to determine depth and is-last-vec. Marks node
as prefixed to avoid duplication."
  (let* ((meta (org-roam-tree--get-node-metadata (point)))
         (is-last-vec (plist-get meta :is-last))
         (prefixed (plist-get meta :prefixed))
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
      (org-roam-tree--store-node-metadata start depth is-last-vec)

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

            (insert
             (if (and (aref is-last-vec depth)
                      (looking-at-p "^[\\s|]*$"))
                 ;; end-of-branch spacer
                 (org-roam-tree-make-prefix (1- depth) nil nil)
               (org-roam-tree-make-prefix depth nil is-last-vec)))))))))



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


(provide 'org-roam-tree)
;;; org-roam-tree.el ends here
