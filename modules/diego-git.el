;; -*- lexical-binding: t; -*-
;;** Git
;; https://github.com/magit/magit
;; A git client for Emacs.
;; C-t to turn any magit buffer into text-mode.
;; Keybindings: https://github.com/emacs-evil/evil-collection/tree/master/modes/magit
;;*** magit.el
;; Keys:
;; https://github.com/emacs-evil/evil-collection/blob/master/modes/magit/evil-collection-magit.el#L280-L309
(use-package magit
  :after (evil-collection compat)
  :general
  ;; format-next-line: off
  (general-nvmap
   :keymaps 'magit-status-mode-map
   "S" #'magit-stash
   "zt" #'evil-scroll-line-to-top
   "zz" #'evil-scroll-line-to-center
   "zb" #'evil-scroll-line-to-bottom
   "gr" #'magit-refresh)
  ;; format-next-line: off
  (general-nvmap
   :keymaps 'magit-log-mode-map
   "zt" #'evil-scroll-line-to-top
   "zz" #'evil-scroll-line-to-center
   "zb" #'evil-scroll-line-to-bottom
   "gr" #'magit-refresh)
  ;; format-next-line: off
  (general-nvmap
   :keymaps 'magit-status-mode-map
   :prefix ","
   "b" '(:ignore t :which-key "branch")
   "bm" #'(diego/git-create-branch-from-origin-master :which-key "branch of origin/master")
   "bb" #'(diego/git-create-branch-from-origin-main :which-key "branch of origin/main")
   "p" '(:ignore t :which-key "pr")
   "pc" #'diego/create-pr-and-visit-url
   "pC" #'diego/checkout-gh-pr
   "o" #'diego/fetch-and-rebase-onto-origin-main
   "v" #'diego/visit-pull-request-url)
  :config
  (setq evil-collection-magit-state 'normal)
  (evil-collection-magit-setup)

  (setq magit-diff-refine-hunk t) ; show granular diffs in selected hunk
  (setq magit-save-repository-buffers nil) ; Don't autosave repo buffers
  ;; Don't display parent/related refs in commit buffers; they are rarely
  ;; helpful and only add to runtime costs.
  (setq magit-revision-insert-related-refs nil)
  (setq magit-diff-refine-ignore-whitespace nil)
  (setopt magit-format-file-function #'magit-format-file-all-the-icons)
  ;; (setopt magit-format-file-function #'magit-format-file-nerd-icons)

  ; modify git log select buffer
  ;; (setq transient-values '((magit-log:magit-log-select-mode "-n30")))
  ;; (put 'magit-log-mode 'magit-log-default-arguments '("-n30" "--decorate"))
  ;; TODO: @d1egoaz 2023-07-28: https://github.com/d1egoaz/dotfiles/commit/5831d94d27b9217a3c3cc7cbe174ac57470dacaf

  ;; force magit log arguments on some magit modes
  (put 'magit-log-mode 'magit-log-default-arguments '("-n30"))
  (put 'magit-log-select-mode 'magit-log-default-arguments '("-n30"))
  ;; (setq magit-refs-show-commit-count nil
  ;;       magit-revision-show-gravatars nil
  ;;       magit-process-popup-time 0
  ;;       magit-log-arguments '("-n50" "--decorate")  ;; was: '("-n256" "--graph" "--decorate")
  ;;       magit-log-section-arguments  '("-n50" "--decorate") ;; was: ("-n256" "--decorate")
  ;;       magit-log-select-arguments '("-n50" "--decorate")  ;; was: '("-n256" "--decorate")
  ;;       )
  ;; (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  ;; (setq magit-display-buffer-function #'display-buffer) ;;to use window rules
  ;; (setq magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)

  (setq magit-section-initial-visibility-alist
        '((untracked . show) (unstaged . show) (staged . show) (stashes . hide)))

  ;; debug magit performance
  ;; (setq magit-refresh-verbose t)

  (remove-hook 'magit-status-sections-hook #'magit-insert-tags-header)
  (remove-hook 'magit-status-sections-hook #'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook #'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook #'magit-insert-untracked-files)
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook #'magit-insert-status-headers)

  (remove-hook 'magit-status-sections-hook #'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-refs-sections-hook #'magit-insert-tags) ;; remove tags from ref section
  ;; (remove-hook 'server-switch-hook 'magit-commit-diff) ;; remove diff on commiting


  (setq magit-repository-directories '(("~/code/" . 2) ("~/dotfiles/" . 1)))

  (setq magit-bury-buffer-function 'magit-restore-window-configuration)

  (transient-append-suffix 'magit-fetch "-p" '("-t" "Fetch all tags" ("-t" "--tags")))

  (defun diego/git-create-branch-from-origin-main ()
    "Create a new branch starting from origin/main."
    (interactive)
    ;;(magit-fetch-branch "origin" "main" nil)
    (magit-call-git "fetch" "origin" "main")
    (let ((new_branch_name
           (read-from-minibuffer "New branch name (from origin/main): " "d1egoaz_")))
      (magit-git-command-topdir (concat "git checkout -b " new_branch_name " origin/main"))))

  (defun diego/git-create-branch-from-origin-master ()
    "Create a new branch starting from origin/master."
    (interactive)
    (magit-fetch-branch "origin" "master" nil)
    (let ((new_branch_name
           (read-from-minibuffer "New branch name (from origin/master): " "d1egoaz_")))
      (magit-git-command-topdir (concat "git checkout -b " new_branch_name " origin/master"))))

  (defun diego/checkout-gh-pr (pr)
    "Checkouts a branch from a PR number or URL."
    (interactive "sPR number or URL: ")
    (magit-git-command-topdir (format "gh pr checkout %s" pr)))

  (defun diego/fetch-and-rebase-onto-origin-main ()
    (interactive)
    ;; (magit-fetch-branch "origin" "main" nil)
    ;; (magit-fetch-branch "origin" "main" nil)
    (magit-run-git "fetch" "origin" "main")
    ;; (magit-git-rebase "origin/master" "--keep-base"))
    (magit-git-rebase "origin/main" nil))

  (defun diego/visit-pull-request-url ()
    "Visit the current branch's PR on Github. Uses gh and magit"
    (interactive)
    (call-process "gh"
                  nil
                  0 ; <- Discard and don't wait for process to finish
                  nil "pr" "view" (magit-get-current-branch) "-w"))

  (defun diego/create-pr-and-visit-url ()
    (interactive)
    (call-interactively 'forge-create-pullreq)
    (diego/visit-pull-request-url))

  (defun diego/insert-jira-info (url)
    "Insert a formatted string with JIRA ticket ID and URL from a given URL."
    (interactive "sEnter JIRA URL: ")
    (let ((ticket-id
           (when (string-match ".*/browse/\\(.*\\)" url)
             (match-string 1 url))))
      (when ticket-id
        (insert ticket-id)
        (save-excursion
          (insert (format "\n\n### Jira Ticket\n%s\n\n" url))
          (insert "## What?\n\n")
          (insert "## Why?\n"))))))

;;*** transient.el
;; Package `transient' is the interface used by Magit to display popups.
;; TODO remove use package as it's now part of Emacs
(use-package transient
  :config
  ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
  ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
  ;; for discussion.
  (transient-bind-q-to-quit)
  ;; Close transient with ESC
  (keymap-set transient-map "ESC" #'transient-quit-one))

;;*** git-link.el
(use-package git-link
  :commands git-link
  :config (setq git-link-open-in-browser t))

;;*** forge.el
(use-package forge
  :after magit
  :init
  ;; https://github.com/emacs-evil/evil-collection/issues/543
  (setq forge-add-default-sections nil)
  (setq forge-add-default-bindings nil))

(provide 'diego-git)
