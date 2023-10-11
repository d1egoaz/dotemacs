;;* Let's Be Evil (evil-*.el)

;; - Cutting and Pasting ::
;; In emacs, cutting is called killing. Pasting is called yanking.
;; - Point and Mark ::
;; The point refers to the cursor. The mark refers to the other side of a selected region (the “active region”).
;; - Guides ::
;; https://github.com/noctuid/evil-guide
;; [[https://github.com/emacs-evil/evil][evil]] is a 'vi' layer for Emacs.

(use-package evil
  :init
  (setq evil-search-module 'isearch)
  (setq evil-kill-on-visual-paste nil) ; don't add the replaced text to the kill ring
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil) ; so we can use evil-collection
  (setq evil-want-minibuffer nil) ; after trying several times, it doesn't fit my workflow
  (setq evil-want-C-u-delete nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-delete t) ; when insert mode
  (setq evil-want-Y-yank-to-eol t) ; behave like y$
  ;; (setq evil-undo-system 'undo-redo) ; default to natively Emacs 28
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-fine-undo t)
  ;; search for the symbol instead of the word when using `*`
  (setq evil-symbol-word-search t)
  :config
  ;; set up some basic equivalents for vim mapping functions. This creates
  ;; global key definition functions for the evil states.
  (general-evil-setup t) ; https://github.com/noctuid/general.el#vim-like-definers
  (evil-put-command-property #'evil-goto-definition :jump t)

  ;;** evil Tuning
  ;;*** Change cursor color evil-mode
  (setq evil-insert-state-cursor '((bar . 2) "#ff00ff"))
  (setq evil-normal-state-cursor '(box "#ff00ff"))

  ;;*** Stay on the original character when leaving insert mode
  (setq evil-move-cursor-back nil)
  (setq evil-shift-round nil)

  ;;*** Make magit commit buffer start in insert mode
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (add-hook 'backtrace-mode-hook #'evil-normal-state)

  ;; n=nzz
  (defun diego--my-center-line (&rest _)
    (evil-scroll-line-to-center nil))
  (advice-add #'evil-search-next :after #'diego--my-center-line)
  ;; N=Nzz
  (advice-add #'evil-search-previous :after #'diego--my-center-line)

  ;; keep J centered
  (evil-put-command-property #'evil-join :move-point nil)
  (defun diego--my-save-position-line (fn &rest args)
    (save-excursion (apply fn args)))
  (advice-add #'evil-join :around #'diego--my-save-position-line)

  ;; from: https://macs.stackexchange.com/a/48721
  (defun my/evil-shift-right ()
    (interactive)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun my/evil-shift-left ()
    (interactive)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))

  ;; Copied from doom emacs
  (defun +evil/window-split-and-follow ()
    "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
    (interactive)
    (let ((evil-split-window-below (not evil-split-window-below)))
      (call-interactively #'evil-window-split)))

  (defun +evil/window-vsplit-and-follow ()
    "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
    (interactive)
    (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
      (call-interactively #'evil-window-vsplit)))

  (evil-mode 1))

;;** evil-collection.el
;; [[https://github.com/emacs-evil/evil-collection][evil-collection]] are Evil bindings for the parts of Emacs that Evil does not cover properly by default, such as
;; help-mode, M-x calendar, Eshell and more. Some bindings don't make sense, so I'm just enabling it per mode.
;; Motion ([, ], {, }, (, ), gj, gk, C-j, C-k)
(use-package evil-collection
  :after evil
  :demand t
  :commands (evil-collection-dired-setup evil-collection-magit-setup)
  :init
  (setq evil-collection-company-use-tng nil) ; I don't want that completion experience
  (setq evil-collection-mode-list nil) ; I don't want surprises, I'll enable it manually by mode
  (setq evil-collection-key-blacklist '("SPC" "SPC m" "C-SPC" "M-SPC" "gd" "gf" "K" "gr" "gR" "[" "]" "gz" "<escape>"))
  (setq evil-collection-setup-minibuffer nil) ; don't setup Vim style bindings in the minibuffer.
  (setq evil-collection-setup-debugger-keys nil)
  (setq evil-collection-calendar-want-org-bindings t)
  :config
  ;; https://github.com/emacs-evil/evil-collection/blob/master/modes/
  (evil-collection-init '(
                          calendar comint company compile consult corfu
                          diff-mode dired docview
                          embark ediff eglot elfeed elisp-mode elisp-refs eshell
                          flycheck flymake
                          go-mode
                          help helpful
                          ibuffer info imenu imenu-list
                          magit ocurr popup
                          vc-annotate vc-dir vc-git
                          vertico vterm wgrep which-key xref)))

;;** goto-chg.el
;; | Keymap | Command                  |
;; |--------+--------------------------|
;; | g;     | goto-last-change         |
;; | g,     | goto-last-change-reverse |
(use-package goto-chg :after evil)

;;** evil-args.el
;; Motions and text objects for delimited arguments.
;; For example, =cia~ (~ia= inner arg) transforms:
;; #+begin_example
;; function(ar|g1, arg2, arg3)
;; function(|, arg2, arg3)
;; #+end_example
;; =daa= (=aa= outer arg) transforms:
;; #+begin_example
;; function(ar|g1, arg2, arg3)
;; function(|arg2, arg3)
;; #+end_example
(use-package evil-args
  :after evil
  :config
  ;; bind evil-args text objects
  (keymap-set evil-inner-text-objects-map "a" 'evil-inner-arg)
  (keymap-set evil-outer-text-objects-map "a" 'evil-outer-arg)
  ;; bind evil-forward/backward-args
  (keymap-set evil-normal-state-map "L" 'evil-forward-arg)
  (keymap-set evil-normal-state-map "H" 'evil-backward-arg)
  (keymap-set evil-motion-state-map "L" 'evil-forward-arg)
  (keymap-set evil-motion-state-map "H" 'evil-backward-arg)

  (define-key evil-motion-state-map (kbd "TAB") 'evil-toggle-fold)
  ;; bind evil-jump-out-args
  ;; (keymap-set evil-normal-state-map "K" 'evil-jump-out-args)

  (setq evil-args-delimiters '("," ";")); include space to use in lisp

  (defun diego--fix-evil-args-lisp ()
    (make-local-variable 'evil-args-delimiters)
    (setq evil-args-delimiters " "))

  :bind (:map
         evil-inner-text-objects-map
         ("a" . #'evil-inner-arg)
         :map
         evil-outer-text-objects-map
         ("a" . #'evil-outer-arg)
         :map
         evil-normal-state-map
         ("L" . #'evil-forward-arg)
         ("H" . #'evil-backward-arg)
         :map
         evil-motion-state-map
         ("H" . #'evil-backward-arg)
         ("L" . #'evil-forward-arg))
  :hook ((emacs-lisp-mode-hook . diego--fix-evil-args-lisp)
         (org-mode-hook . diego--fix-evil-args-lisp)))

;;** evil-commentary.el
;; evil-commentary is an Emacs package for evil-mode that intends to make it easy to comment out (lines of) code:
;; | Keymap | Command                             |
;; |--------+-------------------------------------|
;; | gcc    | comment out a line                  |
;; | gc~    | comments out the target of a motion |
;; | gcap   | comment out a paragrah              |
;; | gc     | comment out selection               |
(use-package evil-commentary
  :straight (:build (autoloads native-compile))
  :after evil
  :config
  (evil-commentary-mode 1))

;;** evil-exchange.el
;; Easy text exchange operator for Evil.
;; On the first use, define (and highlight) the first {motion} to exchange. On the
;; second use, define the second {motion} and perform the exchange.
;; =gx= can also be used from visual mode, which is sometimes easier than coming up with the right {motion}
;; | Keymap | Command                   |
;; |--------+---------------------------|
;; | gx     | evil exchange             |
;; | .      | repeat motion to exchange |
;; | gX     | evil exchange cancel      |
(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

;;** evil-goggles.el
(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-pulse t)
  (set-face-attribute 'evil-goggles-default-face nil :inherit 'cursor)
  (setq evil-goggles-duration 0.3)
  (evil-goggles-mode))

;;** evil-snipe.el
(use-package evil-snipe
  :after evil
  :general
  (general-nmap :keymaps 'evil-snipe-local-mode-map
    "s" 'evil-snipe-s
    "S" 'evil-snipe-S)
  :config
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-smart-case t)
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  :hook ((magit-mode-hook . turn-off-evil-snipe-override-mode)
         (after-init-hook . evil-snipe-mode)))

;;** evil-surround.el
;; Add/change surrounding to text objects.
;; | Keymap         | Command                         |
;; |----------------+---------------------------------|
;; | S<textobject>  | Add surrounding in region       |
;; | ys<textobject> | Add surrounding in normal state |
;; | ds<textobject> | Delete surrounding              |
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(provide 'diego-evil)
