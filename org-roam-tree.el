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
;;
;; Add this section with the others in the org-roam buffer:
;;(add-to-list 'org-roam-mode-sections
;;             #'org-roam-tree-backlinks-section t)

(defgroup org-roam-tree nil
  "Tree-style display extensions for Org-roam."
  :group 'org-roam)

(defcustom org-roam-tree-collapse-after-init t
  "Whether to collapse all file-level branches after rendering the Org-roam tree."
  :type 'boolean
  :group 'org-roam-tree)

(cl-defun org-roam-tree-backlinks-section (node &key (section-heading "Backlinks Tree:"))
  "A tree-style backlinks section for NODE, grouping by source file."
  (org-roam-tree-section node :section-heading section-heading :data-getter #'org-roam-tree-backlinks))


(cl-defun org-roam-tree-section (node &key (section-heading "Tree Section:") (data-getter #'org-roam-tree-backlinks))
  "Generalized logic for a tree in the org-roam buffer. Can 
DATA-GETTER is a function that returns a tree in the format:
((parent . (backlink backlink ...)) ...) where parent is a string and
backlinks are org-roam backlink objects"

(with-org-roam-tree-layout
 (when-let ((tree (funcall data-getter node)))
   (magit-insert-section (org-roam-tree-backlinks)
     ;; Top-level heading
     (magit-insert-heading section-heading)
     ;; Iterate over files
     (dolist (file-entry tree)
       (let ((file (car file-entry))
             (nodes (cdr file-entry))i
             (is-last-file (eq file-entry (car (last tree)))))
         ;; File-level section (collapsible)
         (magit-insert-section (org-roam-tree-file file)
           (let ((prefix (org-roam-tree-make-prefix 1 t is-last-file)))
             (magit-insert-heading (concat prefix (file-name-nondirectory file) (format " (%d)" (length nodes)) )))
           
           ;; Iterate over nodes in this file

           (let ((node-count (length nodes)))
             (cl-loop for n in nodes
                      for node-index from 1
                      for is-last-node = (= node-index node-count) do
                      (let ((start (point)))
                        (org-roam-node-insert-section
                         :source-node (org-roam-backlink-source-node n)
                         :point (org-roam-backlink-point n)
                         :properties (org-roam-backlink-properties n))
                        
                        ;; prepend prefix to first line
                        (save-excursion
                          (goto-char start)
                          (org-roam-tree--prefix-node-content (list is-last-file is-last-node))
                          )))))))))
 (when org-roam-tree-collapse-after-init
   (org-roam-tree-collapse-all-files)
   (goto-char (point-min))
   )))

(defmacro with-org-roam-tree-layout (&rest body)
  "Ensure proper visual layout for Org-roam tree rendering.

- Selects the Org-roam buffer window.
- Temporarily adds a right margin for tree prefixes to avodi a race
  condition between inserting buffer prefixes and visual-line reflow
- Restores the original margins afterward.

BODY is the code that renders the tree content."
  `(with-selected-window (get-buffer-window org-roam-buffer)
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
                             (cdr old-margin))))))


(defun org-roam-tree--prefix-node-content (is-last-list)
  "Insert tree prefixes for a node's rendered content.

IS-LAST-LIST is a list of booleans indicating whether each depth
level is the last sibling."
      ;; First visual line
      (insert (org-roam-tree-make-prefix
               (length is-last-list)
               t
               is-last-list))

      ;; Subsequent visual lines
      (while (and (not (eobp)) (line-move-visual 1 t))

        (unless (= (point) (point-max))
          (beginning-of-visual-line)

          ;; remove stray newline between heading/body
          (save-excursion
            (backward-char)
            (when (eq (char-after) ?\n)
              (delete-char 1)))

          (insert
           (if (and (car (last is-last-list))
                    (looking-at-p "^\\s-*$"))
               ;; end-of-branch spacer
               (org-roam-tree-make-prefix (1- (length is-last-list)) nil nil)
             (concat "\n"
                     (org-roam-tree-make-prefix
                      (length is-last-list)
                      nil
                      is-last-list)))))))


(defun org-roam-tree-make-prefix (depth is-node is-last)
  "Generate a tree-style prefix string for a line.

DEPTH is the nesting level (1 = file).
IS-NODE is t if this is a child node.
IS-LAST may be a boolean or a list of booleans indicating whether
each depth level is the last sibling. If boolean, expand to a list of
(nil nil nil ... is-last); so assumes only the deepest level is specified
and the branch is not last at any other level"
  (let* ((is-last-list
          (if (listp is-last)
              is-last
            (append (make-list (1- depth) nil)
                    (list is-last))))
         (prefix ""))

    ;; vertical guides for ancestor levels
    (dotimes (i (1- depth))
      (setq prefix
            (concat prefix
                    (if (nth i is-last-list)
                        "   "   ; no vertical line if last at that level
                      "│  "))))

    ;; current node connector
    (setq prefix
          (concat prefix
                  (cond
                   ((and is-node (car (last is-last-list))) "└─ ")
                   (is-node "├─ ")
                   ((not (car (last is-last-list))) "│ ")
                   (t "  "))))

    prefix))


(defun org-roam-tree-backlinks (&optional node)
  "Return backlinks of NODE grouped by source file.

Return value:
  ((FILE . (BACKLINK BACKLINK ...)) ...)

NODE defaults to `org-roam-node-at-point` if nil."
  (require 'org-roam)
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

(defun org-roam-tree-collapse-all-files ()
  "Collapse all top-level file branches in the Org-roam tree buffer."
  (interactive)
  (when (derived-mode-p 'org-roam-mode)
    (goto-char (point-min))
    (condition-case nil
        (while (< (line-number-at-pos) (line-number-at-pos (point-max)))
          (next-line)
          (magit-section-hide (magit-current-section)))
      (error (message "done")))))


(provide 'org-roam-tree)
;;; org-roam-tree.el ends here
