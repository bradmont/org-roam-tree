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
;; organizes backlinks by their org file source. May extend in the future to
;; allow for different groupings and deeper trees, for example, to also follow
;; the org tree from the files containing backlinks, or maybe arbitrarily grouped
;; results, which could be helpful with org-roam-ql. As it stands, you can
;; reuse the display function for other purposes if you change what
;; =org-roam-tree-backlinks= returns.
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
(with-selected-window (get-buffer-window org-roam-buffer)
  ;; insert your content and prefixes here

  (when-let ((tree (org-roam-tree-backlinks node)))
    (magit-insert-section (org-roam-tree-backlinks)
      ;; Top-level heading
      (magit-insert-heading section-heading)
      ;; Ugly hack: inserting text prefixes in the buffer leads to a race condition
      ;; with visual-line-mode reflowing the buffer, making some lines overflow, so
      ;; we temporarily set a right margin of 6 chars to leave space for the prefixes.
      ;; TODO : for future iterations with greater depth trees, calculate the
      ;; margin width.
      (let ((old-margin (window-margins)))  ; save existing margins
        (unwind-protect
            (progn
              ;; Set temporary right margin to 6 columns
              (set-window-margins (selected-window) (car old-margin) (+ (or (cdr old-margin) 0) 6))
              ;; Iterate over files
              (dolist (file-entry tree)
                (let ((file (car file-entry))
                      (nodes (cdr file-entry)))
                  ;; File-level section (collapsible)
                  (magit-insert-section (org-roam-tree-file file)
                    (let ((prefix (org-roam-tree-make-prefix 1 t nil)))
                      (magit-insert-heading (concat prefix (file-name-nondirectory file) (format " (%d)" (length nodes)) )))
                    
                    ;; Iterate over nodes in this file

                    (let ((node-count (length nodes)))
                      (cl-loop for n in nodes
                               for node-index from 1
                               for is-last-node = (= node-index node-count) do
                               (let ((start (point))
                                     (prefix (org-roam-tree-make-prefix 2 t is-last-node)))
                                 (org-roam-node-insert-section
                                  :source-node (org-roam-backlink-source-node n)
                                  :point (org-roam-backlink-point n)
                                  :properties (org-roam-backlink-properties n))
                                 
                                 ;; prepend prefix to first line
                                 (save-excursion
                                   (goto-char start)
                                   ;; First visual line gets the branch prefix
                                   (insert (org-roam-tree-make-prefix 2 t is-last-node))
                                   ;; Move down visual lines for wrapped content
                                   (while (line-move-visual 1 t) ;; move 1 visual line, no error
                                     (unless (= (point) (point-max))

                                       (beginning-of-visual-line)
                                       (save-excursion ; there's a \n between node heading and body; delete it to
                                        ; avoid blank lines
                                         (backward-char)
                                         (when (eq (char-after) ?\n)
                                           (delete-char 1)))
                                       ( if (and is-last-node (looking-at-p "^\\s-*$")) ; empty lines are between nodes
                                           (insert (concat "" (org-roam-tree-make-prefix 1 nil nil)))
                                         (insert (concat "\n" (org-roam-tree-make-prefix 2 nil is-last-node)))))
                                     ))
                                 )))))))
          (set-window-margins (selected-window)
                              (car old-margin)
                              (cdr old-margin)))))
    (when org-roam-tree-collapse-after-init
      (org-roam-tree-collapse-all-files)
      (goto-char (point-min))
      )
    )))


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
