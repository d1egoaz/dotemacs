;; * Mode line

(use-package emacs
  :ensure nil
  :config (setq mode-line-compact t)

  ;; ** Format

  ;; ** Show column number

  (column-number-mode 1) ; Show column number next to line number in mode line
  (setq mode-line-position-column-line-format '(" (Ln:%l, Col:%c)"))
  (setq evil-mode-line-format nil)

  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  ;;"[" (:eval (diego/current-tab-name)) "]"
                  ;; " "
                  ;; (:eval (if (eq (buffer-local-value 'major-mode (current-buffer)) 'kubel-mode)
                  ;;            (kubel-current-state)))
                  mode-line-buffer-identification " "
                  ;; default-directory
                  mode-line-position minions-mode-line-modes
                  ;; (vc-mode vc-mode) " "
                  ;; mode-line-misc-info
                  ;; mode-line-mule-info
                  ;; mode-line-client
                  ;; mode-line-modified
                  ;; mode-line-remote
                  mode-line-frame-identification mode-line-end-spaces)))

(use-package minions
  :config
  (setq minions-mode-line-lighter "+")
  (setq minions-direct '(flymake-mode lsp-mode compilation-shell-minor-mode diego/dedicated-mode))
  (minions-mode 1))

;; ** Show current command in the mode line (keycast.el)

;; Keycast mode shows the current command and its key or mouse binding in the mode
;; line, and updates them whenever another command is invoked.

(use-package moody) ; required by keycast window predicate

(use-package keycast
  :after moody
  :config
  (setq keycast-window-predicate 'moody-window-active-p)
  (setq keycast-remove-tail-elements nil) ; leave mode line alone

  ;; copied from Prot
  (dolist (input '(self-insert-command org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event
           '(mouse-event-p mouse-movement-p
                           mwheel-scroll
                           ;; added these additional events
                           lsp-ui-doc--handle-mouse-movement ignore))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global
    t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  ;; (add-to-list 'global-mode-string '("" mode-line-keycast " "))
  (set-face-attribute 'keycast-key nil :height 1.0)
  (set-face-attribute 'keycast-command nil :height 0.5)
  ;; (keycast-mode 1)
  )

;;** Show date and time
(use-package time
  :init
  (setq display-time-format "%Y-%m(%b)-%d(%a) %I:%M%p %Z")
  (setq display-time-interval 17)
  (setq display-time-default-load-average nil))

(provide 'diego-mode-line)
