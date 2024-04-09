;;* Editor

(use-package emacs
  :straight (:type built-in)
  :config
  ;;** Default coding system
  (set-default-coding-systems 'utf-8)

  ;;** New line at EOF
  ;; Add a newline automatically at the end of the file.
  (setq require-final-newline t)

  (setq sentence-end-double-space nil) ; who uses this?

  ;;** Display line numbers and truncated lines

  ;; Explicitly define a width to reduce the cost of on-the-fly computation
  (setq-default display-line-numbers-width 3)

  ;; Show absolute line numbers for narrowed regions to make it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen t)

  ;; Enable line numbers in most text-editing modes.
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)

  (setq-default truncate-lines t)
  (setq-default truncate-partial-width-windows nil)
  (setq-default visual-line-mode nil)

  (setq show-trailing-whitespace t) ;; highlight whitespace at the end of line

  ;; ** Kill ring not save duplicates
  ;; Remove duplicates in the kill ring.
  (setq kill-do-not-save-duplicates t)

  (defun my-truncate-lines-disable ()
    (let ((inhibit-message t))
      (setq truncate-lines t)))

  (add-hook 'prog-mode-hook #'my-truncate-lines-disable)

  ;;** Highlight current line
  (global-hl-line-mode 1)

  ;;** Avoid performance issues with long lines
  ;; When the lines in a file are so long that performance could suffer to an unacceptable degree, we say
  ;; "so long" to the slow modes and options enabled in that buffer, and invoke something much more basic
  ;; in their place.
  (global-so-long-mode 1)

  ;; sometimes when I'm in vim edit mode and there is a selection, this makes more sense.
  (setq-default delete-selection-mode t)

  ;;** Indentation, spaces, and tabs
  ;; Favor spaces over tabs.
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;; Make =tabify= and =untabify= only affect indentation. Not tabs/spaces in the middle of a line.
  (setq tabify-regexp "^\t* [ \t]+")
  ;;** Enable narrowing functions
  (put 'narrow-to-defun 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'narrow-to-region 'disabled nil))

;;** Whitespace cleanup on buffer save
(use-package whitespace
  :straight (:type built-in)
  :hook (before-save-hook . whitespace-cleanup))

;;** expand-region.el
;; SPC >
;; https://github.com/magnars/expand-region.el
;; Emacs extension to increase selected region by semantic units.
;; er/expand-region
;; (use-package expand-region)

;; TODO: test
(use-package expreg
  :straight (:host github :repo "casouri/expreg"))

;;** Parens (elec-pair.el)
(use-package elec-pair
  :straight (:type built-in)
  :config
  (setq electric-pair-pairs
        '((?\" . ?\")
          (?\` . ?\`)
          (?\( . ?\))
          (?\{ . ?\})))
  (setq electric-pair-text-pairs electric-pair-pairs)
  (setq electric-pair-inhibit-predicate (lambda (c) (char-equal c ?\<)))
  (electric-pair-mode 1))

;;** format-all.el
;; Lets you auto-format source code in many languages using the same command for all languages, instead
;; of learning a different Emacs package and formatting command for each language.
(use-package format-all
  :config
  (define-format-all-formatter
   elisp-autofmt
   (:executable)
   (:install "M-x package-install elisp-autofmt")
   (:languages "Emacs Lisp")
   (:features region)
   (:format
    (format-all--buffer-native
     'elisp-autofmt-mode
     (if region
         (lambda () (funcall 'elisp-autofmt-region (car region) (cdr region)))
       (lambda () (funcall 'elisp-autofmt-region (point-min) (point-max))))))))

;;** highlight-parentheses.el
(use-package highlight-parentheses
  :after modus-themes
  :config

  (modus-themes-with-colors
   ;; And here we pass only foreground colors while disabling any
   ;; backgrounds.
   (setq highlight-parentheses-colors (list green-intense magenta-intense blue-intense red-intense))
   (setq highlight-parentheses-background-colors nil))

  ;; Include this if you also want to make the parentheses bold:
  ;; (set-face-attribute 'highlight-parentheses-highlight nil :inherit 'bold)
  (global-highlight-parentheses-mode 1))

;;** lin.el
(use-package lin
  :straight (:host github :repo "protesilaos/lin")
  :init
  (setq lin-mode-hooks
        '(dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          kubel-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          occur-mode-hook
          tabulated-list-mode-hook))
  :config
  (setq lin-face 'lin-blue-override-fg)
  (lin-global-mode 1))

;;** highlight-indent-guides.el
;; it breaks -ts-modes with:
;; Error during redisplay: (jit-lock-function 1) signaled (void-function nil)
;; https://github.com/DarthFennec/highlight-indent-guides/issues/123
(use-package highlight-indent-guides
  :after modus-themes
  :config (setq highlight-indent-guides-method 'character)

  (defun my-modus-themes-highlight-indent-guides ()
    (modus-themes-with-colors
     (custom-set-faces `(highlight-indent-guides-character-face ((,c :foreground ,border))))))

  :hook
  ((prog-mode-hook . highlight-indent-guides-mode)
   (modus-themes-after-load-theme-hook . my-modus-themes-highlight-indent-guides)))

;;** pulsar.el (based on pulse.el)
;; Never lose the cursor again.
(use-package pulsar
  :straight (:host github :repo "protesilaos/pulsar")
  :init
  (setq pulsar-pulse-functions
        '(backward-page
          bookmark-jump
          delete-other-windows
          delete-window
          evil-avy-goto-line
          evil-goto-definition
          evil-mouse-drag-region
          evil-scroll-down
          evil-scroll-line-to-bottom
          evil-scroll-line-to-center
          evil-scroll-line-to-top
          evil-scroll-up
          evil-window-left
          evil-window-right
          evil-window-split
          forward-page
          handle-select-window
          kill-current-buffer
          lsp-find-references
          lsp-find-definition
          lsp-find-implementation
          move-to-window-line-top-bottom
          other-window
          recenter-top-bottom
          reposition-window
          scroll-down
          scroll-down-command
          scroll-up
          scroll-up-command
          tab-close
          tab-new
          tab-next
          windmove-down
          windmove-left
          windmove-right
          windmove-swap-states-down
          windmove-swap-states-left
          windmove-swap-states-right
          windmove-swap-states-up
          windmove-up
          xref-find-references
          xref-find-definitions))

  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.2)
  (setq pulsar-iterations 6)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-magenta)
  (pulsar-global-mode 1))

;;** rainbow-mode.el
(use-package rainbow-mode)

(use-package substitute
  :config
  (setq substitute-fixed-letter-case t)
  (add-hook 'substitute-post-replace-functions #'substitute-report-operation))


;;** Highlight TODO, FIXME, NOTE, etc.
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces '(("TODO" . "#cc9393") ("FIXME" . "#cc9393") ("NOTE" . "#d0bf8f")))
  (global-hl-todo-mode 1))

(provide 'diego-editor)
