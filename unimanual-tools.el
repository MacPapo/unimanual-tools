;;; unimanual-tools.el -- unimanual-tools package

;;; Commentary:
;;; export all .org files recursively from a selected dir
;;; to a selected out with ow-twbs
;;;
;;; Now UniManual is using jekyll, new functions for easy export org
;;; file to md.
;;;
;;; TODO - auto-update the links

;;; Code:

(require 'ox-twbs)
(require 'ox-md)
(require 'ox-publish)

;; Buffer Dynamic export
(defun unimanual-export-md (dir out)
  "Export all org files to md from DIR to OUT."
  (interactive
   "DSelect a directory to export: \nDSelect the destination: ")
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
  (let ((file-list (directory-files-recursively dir
                                                "\\.org$"
                                                t
                                                nil)))
    (dolist (file file-list)
      (with-current-buffer (current-buffer)
        (set-buffer (find-file-noselect file))
        (uni-prepare-org-to-export-md)
        (kill-buffer (current-buffer))))))

(defun uni-prepare-org-to-export-md ()
  "Prepare org to be exported in md."
  (delete-matching-lines "startup: latex")
  (beginning-of-buffer)
  (unless (search-forward
           "#+OPTIONS: toc:nil" nil t)
    (or (search-forward "#+AUTHOR" nil t)
        (search-forward "#+TITLE" nil t))
    (end-of-line)
    (newline)
    (insert "#+OPTIONS: toc:nil author:t date:t num:nil")
    (save-buffer))
  (beginning-of-buffer))

(defun uni-apply-template (dir)
  "Apply template in all files in DIR."
  (interactive
   "DInsert directory: ")
  (let ((file-list (directory-files-recursively dir "\\.md$" t nil))
        (str (apply-template)))
    (with-current-buffer (current-buffer)
      (dolist (file file-list)
        (set-buffer (find-file-noselect file))
        (paste-template str)
        (kill-buffer (current-buffer))))))

(defun apply-template ()
  "Select the template and yank it to the kill ring."
  (with-current-buffer (current-buffer)
    (set-buffer (find-file-noselect (concat (correct-path-template)
                                            "template") t))
    (buffer-substring (point-min) (point-max))))

(defun correct-path-template ()
  "Correct the path to find the template."
  (let ((path default-directory))
    (if (or (string-match-p "/mod1/" path)
            (string-match-p "/mod2/" path))
        (progn
          (if (string-match-p "/mod1/" path)
              (s-replace-regexp "/mod1/"
                                "/"
                                path)
            (s-replace-regexp "/mod2/"
                              "/"
                              path)))
      path)))

(defun paste-template (str)
  "STR in the correct spot the already yanked template."
  (beginning-of-buffer)
  (if (search-forward "date:" nil t)
      (progn
        (end-of-line)
        (newline)
        (insert str)
        (delete-blank-lines))
    (error "ATTENZIONE ERRORE!!")))

(defun prepere-org-to-md (dir)
  "Disable TOC in all org files in the DIR."
  (interactive
   "DSelect a directory: ")
  (pre-export-md dir))

(defun attach-template ()
  "APPLY TEMPLATE."
  (paste-template (apply-template)))

(defun export-current-org-to-md-post ()
  "Export current org file to md post in jekyll porjet."
  (interactive)
  (unless (not (string= "org"
                        (file-name-extension (buffer-file-name))))
    (uni-prepare-org-to-export-md)
    (org-md-export-to-markdown)
    (with-current-buffer (current-buffer)
      (let* ((origin-name (buffer-file-name))
             (md-file (s-replace ".org" ".md" origin-name))
             (tmd-file (concat
                        (s-replace-regexp "org-content"
                                          "_posts"
                                          (file-name-directory md-file))
                        (concat
                         (format-time-string "%Y-%m-%d")
                         "-"
                         (file-name-nondirectory md-file))))
             (file-dup (directory-files
                        (s-replace-regexp "org-content"
                                          "_posts"
                                          (file-name-directory
                                           origin-name))
                        t
                        (file-name-nondirectory md-file))))
        (unless (eq file-dup nil)
          (dolist (d-file file-dup)
            (let ((b-name (file-name-nondirectory d-file)))
              (if (bufferp (get-buffer b-name))
                  (kill-buffer b-name))
              (delete-file d-file))))
        (rename-file md-file tmd-file)
        (set-buffer (find-file-noselect tmd-file t))
        (uni-attach-all (file-name-nondirectory origin-name))
        (with-current-buffer (current-buffer)
          (set-buffer (find-file-noselect (car (directory-files
                                                (file-name-directory (buffer-file-name))
                                                t
                                                "list.md"))))
          (spawn-post-links)
          (kill-buffer (current-buffer)))
        (kill-buffer (current-buffer))))))

(defun uni-attach-all (org-file)
  "Attach all necessary staff ORG-FILE."
  (with-current-buffer (current-buffer)
    (attach-front-matter)
    (attach-template)
    (convert-img-link)
    (attach-title (find-title-org org-file))
    (save-buffer)))

(defun uni-open-md-out ()
  "OPEN MD FILE."
  (interactive)
  (if (string= "org"
               (file-name-extension (buffer-file-name)))
      (progn
        (find-file-other-frame (car (directory-files
                                     (s-replace-regexp "org-content"
                                                       "_posts"
                                                       (file-name-directory
                                                        (buffer-file-name)))
                                     t
                                     (concat
                                      (file-name-base (buffer-file-name))
                                      ".md")))))
    (error "This is not an org buffer!! IDIOT!")))

(defun find-title-org (b)
  "GET TITLE B."
  (with-current-buffer (current-buffer)
    (set-buffer (get-buffer b))
    (beginning-of-buffer)
    (search-forward "#+title: " nil t)
    (buffer-substring (point) (line-end-position))))

(defun attach-title (str)
  "STR."
  (beginning-of-buffer)
  (if (search-forward "!TITLE" nil t)
      (replace-match str t)))

(defun convert-img-link ()
  "Convert path from the current file into /assets Jekyll forlder."
  (while (or (search-forward "![img](../img" nil t)
             (search-forward "![img](../../img" nil t))
    (beginning-of-line)
    (replace-match "![img](/assets/img")))

(defun uni-spawn-all-links ()
  "Spawn all links in the Markdown buffer."
  (interactive)
  (spawn-post-links))

(defun spawn-post-links ()
  "Spawn all posts links in jekyll project."
  (let ((file-list (directory-files-recursively
                    (file-name-directory (buffer-file-name))
                    "\\.md$" t nil)))
    (save-excursion
      (dolist (file file-list)
        (let ((link (file-name-base file)))
          (unless (string= link
                           (file-name-base (buffer-name)))
            (let* ((link-name (s-capitalized-words
                               (replace-regexp-in-string
                                "[[:digit:]_-]"
                                " "
                                link)))
                   (desc (concat "- ["
                                 link-name
                                 "]")))
              (save-excursion
                (if (or (search-forward desc nil t)
                        (search-backward desc nil t))
                    (progn
                      (unless (search-forward link nil t)
                        (beginning-of-line)
                        (kill-whole-line)
                        (insert desc
                                "({% post_url "
                                link
                                " %})\n")))
                  (insert desc
                          "({% post_url "
                          link
                          " %})"
                          "\n")))))))
      (if (buffer-modified-p) (save-buffer)))))

;; HTML

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
      (let ((new-name (expand-file-name
                       (uni-format-name
                        (file-name-nondirectory file)))))
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
