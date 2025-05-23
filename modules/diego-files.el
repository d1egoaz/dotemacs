;; -*- lexical-binding: t; -*-
(use-package emacs
  :straight (:type built-in)
  :config
  ;; ** Disable lock files

  ;; Disables .#file.ext creation.

  ;;#+begin_src elisp
  (setq create-lockfiles nil)

  (setq save-silently t)

  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

  ;; ** Enable files backup

  (setq delete-by-moving-to-trash t)
  ;; creating backups:
  (setq auto-save-default t)
  (setq backup-by-copying t)
  (setq delete-old-versions t)
  (setq kept-new-versions 6)
  (setq kept-old-versions 2)
  (setq make-backup-files t)
  (setq vc-make-backup-files t)
  (setq version-control t)

  ;; for the ` and , see:
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
  ;; The special marker ‘,’ inside of the argument to backquote indicates a value that isn’t constant.
  (setq backup-directory-alist `(("." . ,(concat no-littering-var-directory "backup"))))
  (defun diego--backup-enable-predicate (name)
    (and (normal-backup-enable-predicate name) (not (string-match-p "\\.gpg\\'" name))))
  (setq backup-enable-predicate #'diego--backup-enable-predicate)

  ;;* Files
  ;;** Don't prompt for confirmation when we create a new file or buffer.
  (setq confirm-nonexistent-file-or-buffer nil)

  ;;** Make Script Files Executable Automatically
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

  ;;** Create missing directories when using find file
  ;;Create missing directories when we open a file that doesn't exist under a directory tree that may not exist.
  ;;#+begin_src elisp
  (defun diego/my-create-non-existent-directory ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory))
             (progn
               (make-directory parent-directory 'parents)
               t)))))
  (add-to-list 'find-file-not-found-functions #'diego/my-create-non-existent-directory)


  ;;** Follow symlinks when opening files.
  (setq vc-follow-symlinks t)
  (setq find-file-visit-truename t)

  ;;** Disable the warning X and Y are the same file
  ;; Which normally appears when you visit a symlinked file by the same name.
  (setq find-file-suppress-same-file-warnings t))

;;** Disable file changed on disk messages
;; Turn the delay on auto-reloading from 5 seconds down to 1 second.  We have to do this before turning
;; on =auto-revert-mode= for the change to take effect.
(use-package autorevert
  :straight (:type built-in)
  :config
  ;; Revert Dired and other buffers
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-interval 1)
  (setq revert-without-query '(".*")) ; disables prompt
  (global-auto-revert-mode 1))

;;** Restore cursor to last visited place in a file
;; This means when you visit a file, point goes to the last place where it was when you previously
;; visited the same file.
(use-package saveplace
  :straight (:type built-in)
  :config
  (setq-default save-place t)
  (save-place-mode 1))

;;** dired
(use-package dired
  :straight (:type built-in)
  :after evil-collection
  :general
  ;; format-next-line: off
  (general-nvmap
   :keymaps 'dired-mode-map
   :prefix ","
   "w" #'wdired-change-to-wdired-mode)
  :config
  (evil-collection-dired-setup)
  (setq dired-mouse-drag-files t) ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq insert-directory-program "gls")
  ;; only one dired buffer when opening directories
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-use-ls-dired nil)
  (setq dired-auto-revert-buffer t))

;;*** dired-sidebar.el
(use-package dired-sidebar
  :general
  ;; format-next-line: off
  (general-nvmap
   :keymaps 'dired-sidebar-mode-map
   "<down-mouse-1>" #'dired-sidebar-mouse-subtree-cycle-or-find-file
   "<mouse-2>" #'dired-sidebar-mouse-subtree-cycle-or-find-file
   (kbd "<return>") 'diego/dired-sidebar-find-file
   (kbd "RET") 'diego/dired-sidebar-find-file)
  :config
  (let ((map dired-sidebar-mode-map))
    (define-key map (kbd "TAB") 'dired-sidebar-subtree-toggle)
    (define-key map [tab] 'dired-sidebar-subtree-toggle)
    (define-key map (kbd "RET") 'diego/dired-sidebar-find-file)
    (define-key map (kbd "<return>") 'diego/dired-sidebar-find-file)
    (define-key map (kbd "C-o") 'dired-sidebar-find-file-alt))

  (defun diego/dired-sidebar-find-file ()
    "Wrapper over `dired-sidebar-find-file'."
    (interactive)
    (let ((dired-file-name (dired-get-file-for-visit)))
      (if (file-directory-p dired-file-name)
          (dired-sidebar-subtree-toggle)
        (dired-sidebar-find-file))))

  ;; (set-face-attribute 'dired-sidebar-face nil :height 0.4)
  (setq dired-sidebar-face '(:height 0.8))
  ;; (set-face-attribute 'dired-sidebar-face nil :height 0.8)

  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-should-follow-file t)
  (setq dired-sidebar-follow-file-at-point-on-toggle-open t)
  (setq dired-sidebar-should-follow-file t)
  (setq dired-sidebar-recenter-cursor-on-follow-file nil))

(provide 'diego-files)
