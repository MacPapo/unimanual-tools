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
    (if (eql (select-frame-by-name "xUniManual") nil)
        (message "NON ESISTE")
      (make-frame '((name . "xUniManual")
                    (width . 100)
                    (height . 75))))
    (select-frame-by-name "xUniManual")
    ))

(defun unimanual-view-current-file ()
  "Save and View in default browser the current file buffer already exported."
  (interactive)
  (save-buffer)
  (org-twbs-export-to-html)
  (let* ((file-name (concat (file-name-base (buffer-name)) ".html"))
         (origin-path (expand-file-name file-name))
         (out-name (change-path (url-file-directory (expand-file-name file-name))))
         (out-path (concat out-name file-name)))
    (if (file-exists-p out-path)
        (delete-file out-path))
    (rename-file origin-path out-path nil)
    (browse-url-default-macosx-browser (format "file://%s" out-path))))

(defun change-path (file-path)
  "Correct the FILE-PATH to docs."
  (replace-regexp-in-string "src" "docs" file-path))

(provide 'unimanual-tools)
;;; unimanual-tools.el ends here
