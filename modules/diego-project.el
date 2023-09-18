;;; diego-project.el --- Diego Project Related Functions  -*- lexical-binding: t; -*-

(defvar diego--project-project-roots '("~/work/github.com" "~/work/github.com/1debit/" "~/code/oss" "~/code/tmp"))

;;** project.el
(use-package project
  :straight (:type built-in)
  :config

  ;;** Save Some Buffers default to project root
  (setq save-some-buffers-default-predicate 'save-some-buffers-root)

  (defun prot-project--list-projects ()
    "Produce list of projects in `prot-project-project-roots'."
    (let* ((dirs diego--project-project-roots )
           (dotless directory-files-no-dot-files-regexp)
           (cands (mapcan (lambda (d)
                            (directory-files d t dotless))
                          dirs)))
      (mapcar (lambda (d)
                (list (abbreviate-file-name d)))
              cands)))

  ;; run to update project list
  ;; run project-remember-projects-under on ~
  ;; then run prot-project-add-projects
  (defun prot-project-add-projects ()
    "Append `prot-project--list-projects' to `project--list'."
    (interactive)
    (project--ensure-read-project-list)
    (let ((projects (prot-project--list-projects)))
      (setq project--list (append projects project--list))
      (project--write-project-list)))

  (defun diego--open-readme-and-vterm ()
    (interactive)
    (diego/open-project-readme)
    (diego/vterm-project))

  (defun diego/open-project-readme ()
    "Open the README.md file in a project."
    (interactive)
    (find-file (expand-file-name "README.md" (diego/current-project-root)))
    ;; (dirvish-side))
    (dired-sidebar-show-sidebar))

  (defun diego/open-project-magit ()
    "Open the README.md file in a project."
    (interactive)
    (diego/open-project-readme)
    (with-current-buffer   (find-file (expand-file-name "README.md" (diego/current-project-root)))
      (magit-status)))


  (defun diego/project-compile-dwim (command)
    "Run `compile' in the project root."
    ;; (declare (interactive-only compile))
    (interactive)
    (let ((default-directory (diego/current-project-root))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      (compile command t)))

  (defun diego/project-compile ()
    "Run `compile' in the project root."
    (declare (interactive-only compile))
    (interactive)
    (let ((default-directory (diego/current-project-root))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      ;; use t for enable Comint mode with compilation-shell-minor-mode.
      (compile (completing-read "Compile command: " compile-history nil nil nil 'compile-history) t)))

  (defun diego/recompile ()
    "Function has been almost copied from the original recompile.
It has been modified to always run on comint mode."
    (interactive)
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (let ((default-directory (or compilation-directory default-directory))
          (command (eval compile-command)))
      (apply #'compilation-start (list command t nil nil)))) ; make sure to always use comint mode

  (defun diego/current-project-root ()
    "Return the current project root."
    (when-let ((proj (project-current)))
      (project-root proj)))

  (defun diego/project-generate-ctags ()
    "Regenerate tags, when `prefix-arg' don't generate recursive."
    (interactive)
    (let* ((opts (if current-prefix-arg "" " -R --exclude=.git --exclude=node_modules"))
           (cmd (format "ctags -e %s ." opts)))
      (compile cmd t)))

  (setq project-vc-extra-root-markers '(".git" ".project"))
  (setq project-switch-commands '((project-find-file "Find file" ?f)
                                  (diego/open-project-readme "README.md" ?.)
                                  (consult-ripgrep "Search" ?s)
                                  (project-dired "Dired" ?d)
                                  (diego/open-project-magit "Git status" ?g)
                                  (project-vc-dir "Project vc-dir" ?G)
                                  (diego/consult-buffer-for-project "Recent project buffer" ?R)
                                  (project-shell-command "Shell command" ?!)
                                  (diego--open-readme-and-vterm "Vterm project" ?v))))

(provide 'diego-project)
