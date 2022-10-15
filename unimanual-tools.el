;;; unimanual-tools.el -- unimanual-tools package

;;; Commentary:
;;; export all .org files recursively from a selected dir
;;; to a selected out with ow-twbs

;;; Code:

(require 'ox-twbs)
(require 'ox-md)
(require 'ox-publish)

;; Buffer Dynamic export
(defun unimanual-export-md (dir out)
  "Export all org files to md from DIR to OUT."
  (interactive
   "DSelect a directory to export: \nDSelect the destination: ")
  (message "DIR: %s OUT: %s" dir out)
  (pre-export-md dir)
  (uni-purge-dir out)
  (setq-local org-publish-project-alist
              `(("UniManual"
                 :base-directory ,(format "%s" dir)
                 :publishing-directory ,(format "%s" out)
                 :publishing-function org-md-publish-to-md
                 :with-sub-superscript nil
                 :recursive t
                 )))
  (org-publish-remove-all-timestamps) ;; overwrite old files
  (org-publish-project "UniManual")
  (format-exported-md out))

(defun format-exported-md (dir)
  "Prepend timestamp to all .md exported files in DIR recursively."
  (let ((file-list (directory-files-recursively dir "\\.md$" t nil)))
    (dolist (file file-list)
      (let* ((template "template")
             (time (format-time-string "%Y-%m-%d"))
             (new-name (format "%s/%s"
                               (file-name-directory file)
                               (format "%s-%s"
                                       time
                                       (file-name-nondirectory
                                        file)))))
        (unless (file-equal-p file template)
          (rename-file file new-name t)
          (with-current-buffer (current-buffer)
            (set-buffer (find-file-noselect new-name))
            (beginning-of-buffer)
            (attach-front-matter)
            (save-excursion
              (convert-img-link))
            (save-buffer)
            (kill-buffer (current-buffer))))))))

(defun uni-purge-dir (dir)
  "Delete all files in DIR."
  (let ((file-list (directory-files-recursively dir "\\.md$" t nil))
        (template "template"))
    (message "OUT: %s" dir)
    (dolist (file file-list)
      (unless (file-equal-p file template)
        (delete-file file)))))

(defun attach-front-matter ()
  "Insert Front Matter for Jekyll at the beginning of the buffer."
  (beginning-of-buffer)
  (insert
   "---\n"
   "layout: post\n"
   "title: !TITLE\n"
   "date: " (concat (format-time-string "%Y-%m-%d %H:%M:%S %z") "\n")
   "math: true\n"
   "---\n\n"))

(defun pre-export-md (dir)
  "Disable TOC in all org files in the DIR."
  (let ((file-list (directory-files-recursively dir "\\.org$" t nil)))
    (dolist (file file-list)
      (with-current-buffer (current-buffer)
        (set-buffer (find-file-noselect file))
        (delete-matching-lines "startup: latex")
        (beginning-of-buffer)
        (unless (search-forward
                 "#+OPTIONS: toc:nil author:t date:t num:nil" nil t)
          (or (search-forward "#+AUTHOR" nil t)
              (search-forward "#+TITLE" nil t))
          (end-of-line)
          (newline)
          (insert "#+OPTIONS: toc:nil author:t date:t num:nil")
          (beginning-of-buffer))
        (save-buffer)
        (kill-buffer (current-buffer))))))

(defun uni-apply-template (dir)
  (interactive
   "DInsert directory: ")
  (message "DIR: %s" dir)
  (let ((template "template")
        (file-list (directory-files-recursively dir "\\.md$" t nil)))
    (with-current-buffer (current-buffer)
      (set-buffer (find-file-noselect template))
      (copy-region-as-kill (point-min) (point-max))
      (dolist (file file-list)
        (set-buffer (find-file-noselect file))
        (beginning-of-buffer)
        (if (search-forward "date:" nil t)
            (progn
              (end-of-line)
              (newline)
              (yank)
              (delete-blank-lines)
              (save-buffer))
          (error "ATTENZIONE ERRORE!!"))
        (kill-buffer (current-buffer))))))

(defun prepere-org-to-md (dir)
  "Disable TOC in all org files in the DIR."
  (interactive
   "DSelect a directory: ")
  (pre-export-md dir))

(defun convert-img-link ()
  "Convert path from the current file into /assets Jekyll forlder."
  (while (or (search-forward "![img](../img" nil t)
             (search-forward "![img](../../img" nil t))
    (message "Hey ecco una immagine al punto %d" (point))
    (beginning-of-line)
    (replace-match "![img](/assets/img")))

;; TODO - auto catego and tag

;; Buffer Dynamic export
(defun unimanual-export-html (dir out)
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
  (let ((files (uni-mark-format dir)))
    (dolist (file files)
      (message "%s" file)
      (let ((new-name (expand-file-name
                       (uni-format-name
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

(defun unimanual-open-index-preview ()
  "Open in Safari the UniManual index html."
  (interactive)
  (unless (not (string= "UniManual" (projectile-project-name)))
    (let ((index-name (projectile-expand-root "docs/index.html")))
      (browse-url-default-macosx-browser
       (format "file://%s" index-name)))))

(defun unimanual-view-current-file ()
  "Save and View in Safari the current file buffer already exported."
  (interactive)
  (save-buffer)
  (org-twbs-export-to-html)
  (let* ((file-name (s-replace ".org" ".html" (buffer-file-name)))
         (dest-name (uni-change-path file-name)))
    (unless (not (file-exists-p dest-name))
      (delete-file dest-name))
    (uni-save-new-file-name file-name dest-name)
    (browse-url-default-macosx-browser
     (format "file://%s" dest-name))))

(defun uni-change-path (file-path)
  "Correct the FILE-PATH to docs."
  (replace-regexp-in-string
   "/UniManual/src"
   "/UniManual/docs"
   file-path))

(provide 'unimanual-tools)
;;; unimanual-tools.el ends here
