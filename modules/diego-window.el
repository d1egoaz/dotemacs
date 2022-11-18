;;* Windows

;;** Jump to windows (ace-window.el)
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))

(use-package window
  :straight (:type built-in)
  :config

  ;; read: https://www.masteringemacs.org/article/demystifying-emacs-window-manager
  ;; Great for all those annoying popups where you only care about the contents when something goes wrong.
  ;; (add-to-list 'buffer-display-alist
  ;;      '("\\*compilation\\*" display-buffer-no-window
  ;;         (allow-no-window . t)))
  (defun diego--debug-buffer-alist (b a)
    (message "b:%s" b)
    (print a) nil)

  ;;** Window rules
  ;; The =display-buffer-alist= is a rule-set for controlling the placement of windows.

  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Display-Action-Alists.html
  (setq display-buffer-alist
        `(
          ;; ↑ top side window
          ("\\*\\(Messages\\|world-clock\\|Backtrace\\|Warnings\\|Compile-Log\\|Flymake log\\)\\*"
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . top)
           (window-height . 0.3)
           (dedicated . t)
           (preserve-size . (t . t)))
          ;;️↓ bottom side window
          ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\*Flycheck errors\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.3))
          ((derived-mode . reb-mode) ; M-x re-builder
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 4) ; note this is literal lines, not relative
           (dedicated . t)
           (preserve-size . (t . t)))
          ((derived-mode . compilation-mode)
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.3))
          ;; ← left side window
          ((or . ((derived-mode . help-mode)
                  (derived-mode . apropos-mode)
                  (derived-mode . helpful-mode)))
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . left)
           (window-width . 0.40))
          ;; → right side window
          ("\\*\\(vterm\\|vterm-project\\|VC-history\\).*"
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . right)
           (window-width . 0.50))
          ((or . ((derived-mode . magit-diff-mode)
                  (derived-mode . magit-rev-mode)))
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . right)
           (window-width . 0.50))
          ("\\*Ilist\\*"
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . right)
           (window-width . 0.20))
          ;; below current window
          ("\\*\\(Calendar\\|Org todo\\)\\*"
           (display-buffer-reuse-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer))
          ("\\*Process List\\*"
           (display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.4)
           (slot . 0))
          ;; ***************************
          ;; Workspaces (dedicated tabs
          ;; ***************************
          ("\\*diego/vterm\\*"
           (display-buffer-in-tab)
           (tab-name . "|vterm|"))
           ;;;; Kubel
          ("\\*kubel-process.*"
           (display-buffer-in-tab display-buffer-in-side-window)
           (tab-name . "kubel")
           (side . bottom)
           (window-height . 0.2)
           (slot . 0))
          ("\\*kubel stderr\\*"
           (display-buffer-in-tab display-buffer-in-side-window)
           (tab-name . "kubel")
           (side . bottom)
           (window-height . 0.2)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*kubel resource.*"
           (display-buffer-in-tab display-buffer-in-side-window)
           (tab-name . "kubel")
           (side . right)
           (window-width . 0.5))
          ("\\*kubel manager.*"
           (display-buffer-in-tab)
           (tab-name . "kubel")
           (dedicated . t))
           ;;;; Elfeed
          ("\\*elfeed-search\\*"
           (display-buffer-in-tab)
           (tab-name . "|elfeed|"))
          ("\\*elfeed-entry\\*"
           (display-buffer-in-tab display-buffer-in-side-window)
           (tab-name . "|elfeed|")
           (side . bottom)
           (window-height . 0.7))
           ;;;; Scratch buffers
          ("\\*scratch.*"
           (display-buffer-in-tab)
           (tab-name . "scratch"))
          ("\\*\\(straight-process\\|Async-native-compile-log\\)\\*"
           (display-buffer-in-tab)
           (tab-name . "general"))
          ;;; Automatic workspaces-tabs management
          ;; Every buffer visiting a file goes automatically to a tab given by the root project.
          ;; idea from https://emacs.stackexchange.com/a/64486
          (diego/workspaces-validate-buffer
           (display-buffer-in-tab)
           (tab-name . diego/workspaces-name-for-buffer))
          ;; end display-buffer-alist elements
          ))

  ;; (setq display-buffer-alist nil) ;; use on emergency :)

  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  ;; (setq even-window-sizes nil)     ; avoid resizing
  (setq window-sides-vertical nil)
  ;; apply rules to all including manual switching
  (setq switch-to-buffer-obey-display-actions t)
  ;; (setq switch-to-prev-buffer-skip t)
  (setq switch-to-prev-buffer-skip nil)

  (setq split-height-threshold nil)
  (setq split-width-threshold nil)

  (defun diego/split-window-horizontally-3 ()
    (interactive)
    (delete-other-windows)
    (split-window-horizontally)
    (split-window-horizontally)
    (balance-windows)
    (other-window -1))

  (defun diego/follow-mode-3 ()
    (interactive)
    (diego/split-window-horizontally-3)
    (follow-mode 1))

  (define-minor-mode diego/toggle-window-dedication
    "Minor mode for dedicating windows.
This minor mode dedicates the current window to the current buffer.
The code is taken from here: https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el"
    :init-value nil
    :lighter " [D]"
    (let* ((window (selected-window))
           (dedicated (window-dedicated-p window)))
      (set-window-dedicated-p window (not dedicated))
      (message "Window %sdedicated to %s"
               (if dedicated "no longer " "")
               (buffer-name))))

  ;; from https://www.reddit.com/r/emacs/comments/pka1sm/my_first_package_aside_for_easier_configuration/hc3g1z7
  (cl-defun diego/display-buffer-in-side-window (&optional (buffer (current-buffer)))
    "Display BUFFER in dedicated side window."
    (interactive)
    (let ((display-buffer-mark-dedicated t))
      (display-buffer-in-side-window buffer
                                     '((side . right)
                                       (window-parameters
                                        (no-delete-other-windows . t))))))

  ;; Swap windows if there are two of them
;; copied from https://github.com/karthink/.emacs.d/blob/master/lisp/better-buffers.el
(defun diego/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (cl-first (window-list)))
                (w2 (cl-second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

(defun diego/window-remove-side-parameter ()
  (interactive)
  (set-window-parameter nil 'window-side nil))

  :bind ("<f6>" . #'window-toggle-side-windows))

(use-package windmove
  :straight (:type built-in)
  :config
  (windmove-install-defaults nil '(meta)
                             '((windmove-display-left ?h)
                               (windmove-display-right ?l)
                               (windmove-display-up ?k)
                               (windmove-display-down ?j)
                               (windmove-display-same-window ?m)))
  (windmove-install-defaults nil '(super)
                             '((windmove-left ?h)
                               (windmove-right ?l)
                               (windmove-up ?k)
                               (windmove-down ?j))))

(use-package winner
  :hook
  (after-init-hook . winner-mode))

(provide 'diego-window)
