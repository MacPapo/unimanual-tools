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
  "Take all org files from DIR and rename it in kebab-case."
  (interactive
   "DSelect a directory to analyze: ")
  (let* ((files (uni-mark-format dir)))
    (dolist (file files)
      (message "%s" file)
      (let ((new-name (expand-file-name (uni-format-name
                                         (file-name-nondirectory file)))))
        (message "%s" new-name)
        (uni-save-new-file-name file new-name)))))

(defun uni-mark-format (dir)
  "Find all .org files in DIR recursively and return it in a list."
  (directory-files-recursively dir "\\.org$" t nil))

(defun uni-format-name (name)
  "Downcase and replace all _ with - in NAME."
  (downcase (replace-regexp-in-string "_" "-" name)))

(defun uni-save-new-file-name (old-file-name new-file-name)
  "Compare OLD-FILE-NAME with NEW-FILE-NAME.
If the name is different save it as NEW-FILE-NAME and change
also the buffer name if exists."
  (unless (string= old-file-name new-file-name)
    (rename-file old-file-name new-file-name t)
    (unless (not (bufferp (get-buffer
                           (file-name-nondirectory old-file-name))))
      (with-current-buffer (current-buffer)
        (set-buffer (get-buffer
                     (file-name-nondirectory old-file-name)))
        (set-visited-file-name
         (file-name-nondirectory new-file-name) t t)))))

(defun unimanual-view-current-file ()
  "Save and View in default browser the current file buffer already exported."
  (interactive)
  (save-buffer)
  (org-twbs-export-to-html)
  (let* ((file-name (s-replace ".org" ".html" (buffer-file-name)))
         (dest-name (uni-change-path file-name)))
    (unless (not (file-exists-p dest-name))
      (delete-file dest-name))
    (uni-save-new-file-name file-name dest-name)
    (browse-url-default-macosx-browser (format "file://%s" dest-name))))

(defun uni-change-path (file-path)
  "Correct the FILE-PATH to docs."
  (replace-regexp-in-string "/UniManual/src" "/UniManual/docs" file-path))

(provide 'unimanual-tools)
;;; unimanual-tools.el ends here
