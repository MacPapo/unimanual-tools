;;; unimanual-tools.el -- unimanual-tools package

;;; Commentary:
;;; export all .org files recursively from a selected dir
;;; to a selected out with ow-twbs

;;; Code:

(require 'ox-twbs)
(require 'ox-publish)
(require 'xwidget)

;; Buffer Dynamic export
(defun unimanual-export (dir out)
  "Export all org files to html from DIR to OUT."
  (interactive
   "DSelect a directory to export: \nDSelect the destination: ")
  (setq-local org-publish-project-alist
              `(("UniManual"
                 :base-directory ,(format "%s" dir)
                 :publishing-directory ,(format "%s" out)
                 :publishing-function org-twbs-publish-to-html
                 :with-sub-superscript nil
                 :recursive t
                 )))
  ;; (org-publish-remove-all-timestamps) ;; overwrite old files
  (org-publish-project "UniManual"))

;; Convert snake_case org files to kebab-case
(defun unimanual-apply-name-scheme (dir)
  "Take all files from DIR and rename it with the kebab-case standard."
  (interactive
   "DSelect a directory to analyze: ")
  (message "Dir: %s" dir)
  (uni-mark-format dir))

(defun uni-format-name (str)
  "Uni format string STR."
  (downcase (replace-regexp-in-string "_" "-" (file-name-base str))))

(defun uni-mark-format (dir)
  "Find all .org files in DIR."
  (let ((files (directory-files-recursively dir "\\.org$" t nil)))
    (dolist (file files)
      (rename-file file (concat (url-file-directory file)
                                (uni-format-name file)
                                ".org")))))

(defun unimanual-test-result (file)
  "Test the exported html FILE and links in the xwidget buffer."
  (interactive
   "FGive me the index.html in the /docs file: ")
  (let ((uri (format "file://%s" (expand-file-name file))))
    (make-frame '((name . "UniManual")
                  (width . 100)
                  (height . 75)))
    (select-frame-by-name "UniManual")
    (xwidget-webkit-goto-url uri)))

(provide 'unimanual-tools)
;;; unimanual-tools.el ends here
