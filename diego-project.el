(setq diego-project-project-roots '(
          "~/src/github.com/Shopify"
          "~/code/"
          "~/dotfiles/"))

;; Copied from Manuel Uberti:
;; <https://www.manueluberti.eu/emacs/2020/11/14/extending-project/>.
;;
(cl-defmethod project-root ((project (head local)))
  "Project root for PROJECT with HEAD and LOCAL."
  (cdr project))

(defun mu--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
    (unless (executable-find "fds")
    (error "Cannot find 'fd' command is shell environment $PATH"))
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fdx -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

;;;###autoload
(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (message ">>>>>>>>>>> oe")
  (mapcan #'mu--project-files-in-directory
          (or dirs (list (project-root project)))))

(defun prot-project--list-projects ()
  "Produce list of projects in `prot-project-project-roots'."
  (let* ((dirs diego-project-project-roots )
         (dotless directory-files-no-dot-files-regexp)
         (cands (mapcan (lambda (d)
                          (directory-files d t dotless))
                        dirs)))
    (mapcar (lambda (d)
              (list (abbreviate-file-name d)))
            cands)))

;;;###autoload
(defun diego/open-project-readme ()
  "Open the README.md file in a project."
  (interactive)
  (find-file (expand-file-name "README.md" (project-root (project-current t)))))


(provide 'diego-project)