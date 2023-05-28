(use-package emacs
  :straight (:type built-in)
  :config
  ;; ** Disable lock files

  ;; Disables .#file.ext creation.

  ;;#+begin_src elisp
  (setq create-lockfiles nil)

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
    (and (normal-backup-enable-predicate name)
         (not (string-match-p "\\.gpg\\'" name))))
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
             (progn (make-directory parent-directory 'parents)
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
  (general-nvmap
    :keymaps 'dired-mode-map
    :prefix ","
    "w"  #'wdired-change-to-wdired-mode)
  :config
  (evil-collection-dired-setup)
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq insert-directory-program "gls")
  ;; only one dired buffer when opening directories
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-use-ls-dired nil)
  (setq dired-auto-revert-buffer t))

;;*** dired-sidebar.el
(use-package dired-sidebar
  :general
  (general-nvmap :keymaps 'dired-sidebar-mode-map
    "<mouse-2>" #'dired-sidebar-mouse-subtree-cycle-or-find-file)
  :config
  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-should-follow-file t)
  (setq  dired-sidebar-follow-file-at-point-on-toggle-open t)
  (set-face-attribute 'dired-sidebar-face nil :height 0.8)
  (setq dired-sidebar-should-follow-file t)
  (setq dired-sidebar-recenter-cursor-on-follow-file nil))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config

  (setq dirvish-attributes '(all-the-icons))
  (setq dirvish-preview-dispatchers nil)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-use-header-line nil)
  (setq dirvish-use-mode-line nil)
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (setq dirvish-libraries
        '((dirvish-widgets  path symlink sort omit index free-space file-link-number
                            file-user file-group file-time file-size file-modes
                            file-inode-number file-device-number
                            audio image gif video epub pdf pdf-preface archive)
          ;; (dirvish-vc       vc-state git-msg vc-diff vc-blame vc-log vc-info)
          (dirvish-icons    all-the-icons vscode-icon)
          (dirvish-collapse collapse)
          (dirvish-subtree  subtree-state)
          (dirvish-yank     yank)))

  (dirvish-side-follow-mode 1))

(provide 'diego-files)
