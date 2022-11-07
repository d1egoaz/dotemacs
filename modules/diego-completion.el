;;* Completion

;;** Preserve history with savehist-mode
;; Run =(delete-dups extended-command-history)= for example to delete duplicates
;; from previous history files.
(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode 1)
  :config
  (setq auto-save-interval 100)
  (setq history-delete-duplicates t)
  (setq history-length 1000)
  (setq savehist-additional-variables '(compile-command kill-ring regexp-search-ring)))

;; ** Recent.el
(use-package recentf
  :config
  (setq recentf-max-menu-items 1000)
  (setq recentf-max-saved-items 1000)
  (setq recentf-auto-cleanup nil)
  (add-hook 'after-init-hook #'recentf-mode))

;;** Completion framework (corfu.el)
;;*** corfu
(use-package corfu
  :config
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  (setq corfu-auto-prefix 2)
  (setq corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (setq corfu-min-width 100)
  (setq corfu-max-width corfu-min-width)     ; Always have the same width
  (setq corfu-count 15)
  (setq corfu-scroll-margin 4)
  (setq corfu-echo-documentation nil)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  :bind
  ;; Configure SPC for separator insertion
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("M-m" . corfu-move-to-minibuffer))
  :init
  (global-corfu-mode 1))

;;*** kind-icon
(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;*** corfu-doc
(use-package corfu-doc
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :after corfu
  :general (:keymaps 'corfu-map
                     ;; This is a manual toggle for the documentation window.
                     [remap corfu-show-documentation] #'corfu-doc-toggle ; Remap the default doc command
                     ;; Scroll in the documentation window
                     "M-n" #'corfu-doc-scroll-up
                     "M-p" #'corfu-doc-scroll-down)
  :config
  (setq corfu-doc-delay 0.5)
  (setq corfu-doc-max-width 70)
  (setq corfu-doc-max-height 80)

  ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
  ;; extra-safe that this is set when corfu-doc is loaded. I do not want
  ;; documentation shown in both the echo area and in the `corfu-doc' popup.
  (setq corfu-echo-documentation nil)
  (corfu-doc-mode 1))

;;***  Dabbrev
(use-package dabbrev
  :config
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)

  (defun diego/dabbrev-full-completion ()
    (interactive)
    (let ((current-prefix-arg 16)) ; 16 C-u C-u = all buffers
      (call-interactively #'dabbrev-completion)))

  :bind (("M-/" . diego/dabbrev-full-completion)
         ("C-M-/" . dabbrev-completion)))


(use-package emacs
  :straight (:type built-in)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;;** Recursive Editing
  ;; When in the minibuffer allow using commands that uses the minibuffer
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1) ;; display the recursion level in the minibuffer

  ;; Grow and shrink minibuffer
  (setq resize-mini-windows 'grow-only)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode))

;;** Selection and Narrowing

;; Individual packages that work well together.
;; Vertico, Consult, Embark, Marginalia, and Orderless.

;; All of the above try to use the minibuffer's existing hooks and extension
;; mechanisms, and benefit from large parts of the rest of Emacs using those
;; mechanisms too. Consequently, they all interop with each other and other parts
;; of the Emacs ecosystem. You can pick which you want.

;;*** vertico.el
;; Provides the vertical completion user interface.
(use-package vertico
  :demand t
  :after orderless ; https://github.com/oantolin/orderless/issues/64#issuecomment-868989378
  :straight (:files (:defaults "extensions/*") :includes (vertico-repeat vertico-quick))
  :init
  (add-hook 'vertico-mode-hook (lambda ()
                                 (setq completion-in-region-function
                                       (if vertico-mode
                                           #'consult-completion-in-region
                                         #'completion--in-region))))
  (vertico-mode)
  :config
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  (setq vertico-count 20)

  ;; vertico repeat
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (advice-add #'ffap-menu-ask :around (lambda (&rest args)
                                        (cl-letf (((symbol-function #'minibuffer-completion-help)
                                                   #'ignore))
                                          (apply args))))

  (defun vertico--select-first (state)
    (when (> (alist-get 'vertico--total state) 0)
      (setf (alist-get 'vertico--index state) 0))
    state)
  (advice-add #'vertico--recompute :filter-return #'vertico--select-first)

  :bind (:map
         vertico-map
         ("<C-tab>" . #'vertico-quick-insert)
         ("C-q"     . #'vertico-quick-exit)))



;;*** consult.el
;; Provides a suite of useful commands using completing-read.
;; https://github.com/minad/consult#use-package-example
;; https://github.com/minad/consult/wiki
;; M-m quick select
;; M-i quick insert
;; M-w copy
(use-package consult
  :demand t
  :after vertico
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-line consult-ripgrep consult-git-grep consult-grep
   consult-bookmark
   ;; consult-recent-file
   consult-xref
   consult--source-project-recent-file
   ;; consult--source-recent-file
   consult--source-project-recent-file consult--source-bookmark
   :preview-key '(:debounce 0.2 any))
  ;; :preview-key (list (kbd "M-SPC") (kbd "C-M-j") (kbd "C-M-k")))


  (setq consult-narrow-key ">")
  (setq consult-widen-key "<")

  ;; TODO 2021-09-14T22:38:11Z
  ;; consult-line-start-from-top

  ;; disable fd for now until https://github.com/minad/consult/wiki#find-files-using-fd
  ;; (setq consult-find-args "fd --color=never --full-path ARG OPTS")
  ;; add --hidden
  (setq consult-ripgrep-args "rg --hidden --glob=!.git/ --glob=!TAGS --null --line-buffered --color=never --max-columns=1000 --path-separator=/ --smart-case --no-heading --line-number .")
  (setq xref-search-program 'ripgrep)

  (setq consult-fontify-preserve t)
  (setq consult-preview-key nil)
  ;; (setq consult-project-root-function #'vc-root-dir)
  (setq consult-project-root-function #'diego/current-project-name)

  (defun diego/search-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  (defun diego--recentf-mode-consult ()
    (recentf-mode 1))
  (advice-add 'diego--recentf-mode-consult :before #'consult-recent-file)

  ;; Makes only the open buffers list visible when calling consult-buffer command
  ;; by hiding the other sources, but still allowing the narrowing to recent
  ;; files (by typing f SPC), bookmarks (m SPC) and project buffer and/or files
  ;; (p SPC).  Hide all sources, except normal buffers in consult-buffer by
  ;; default
  (dolist (src consult-buffer-sources)
    (unless (eq src 'consult--source-buffer)
      (set src (plist-put (symbol-value src) :hidden t))))

  (defun buffer-list-for-mode (mode)
    (seq-filter (lambda (buffer)
                  (eq mode (buffer-local-value 'major-mode buffer)))
                (buffer-list)))

  (defvar kubel-buffer-source
    `(:name     "Kubel"
                :narrow   ?k
                :category buffer
                :state    ,#'consult--buffer-state
                :items  ,(lambda () (mapcar #'buffer-name (buffer-list-for-mode 'kubel-mode)))))
  (add-to-list 'consult-buffer-sources 'kubel-buffer-source 'append)

  (defvar vterm-buffer-source
    `(:name     "Vterm"
                :narrow   ?v
                :category buffer
                :state    ,#'consult--buffer-state
                :items  ,(lambda () (mapcar #'buffer-name (buffer-list-for-mode 'vterm-mode)))))
  (add-to-list 'consult-buffer-sources 'vterm-buffer-source 'append)

  (defun diego/consult-buffer-for-project ()
    (interactive)
    (require 'consult)
    ;; start with initial narrowing of `p`: project
    (setq unread-command-events (append unread-command-events (list ?p 32)))
    (consult-buffer))

  (defun my/consult-line-forward ()
    "Search for a matching line forward."
    (interactive)
    (consult-line))

  :bind (
         ([remap apropos]                       . #'consult-apropos)
         ([remap bookmark-jump]                 . #'consult-bookmark)
         ([remap evil-show-marks]               . #'consult-mark)
         ([remap goto-line]                     . #'consult-goto-line)
         ([remap imenu]                         . #'consult-imenu)
         ([remap load-theme]                    . #'consult-theme)
         ([remap locate]                        . #'consult-locate)
         ([remap org-goto]                      . #'consult-org-heading)
         ([remap switch-to-buffer]              . #'consult-buffer)
         ([remap switch-to-buffer-other-window] . #'consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . #'consult-buffer-other-frame)
         ([remap yank-pop]                      . #'consult-yank-pop)
         ([remap recentf-open-files]            . #'consult-recent-file)
         ("C-s" . #'my/consult-line-forward)
         :map minibuffer-local-map
         ("C-r" . consult-history)))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package consult-yasnippet
  :after consult)

(use-package consult-dir
  :after (vertico consult)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; https://github.com/gagbo/consult-lsp
(use-package consult-lsp
  :after (consult lsp-mode)
  :bind
  (:map
   lsp-mode-map
   ([remap xref-find-apropos] . #'consult-lsp-symbols)))


;;*** embark.el
;; Embark is a minor mode to allow each minibuffer entry to have multiple actions.
;; https://github.com/oantolin/embark
;; https://github.com/oantolin/embark/wiki/Default-Actions
(use-package embark
  :after wgrep
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config

  ;; If you want to see the actions and their key bindings, but want to use the
  ;; key bindings rather than completing the command name
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun diego/embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.
    Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
    (interactive)
    (pcase-let ((`(,type . ,candidates)
                 (run-hook-with-args-until-success 'embark-candidate-collectors)))
      (pcase type
        ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                         (embark-export)))
        ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
                 (embark-export)))
        ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                             (embark-export)))
        (x (user-error "embark category %S doesn't support writable export" x)))))

  (defun +embark-collect-hook ()
    (when (eq embark-collect--kind :live)
      (with-selected-window (active-minibuffer-window)
        (setq-local vertico-resize t vertico-count 0)
        (vertico--exhibit))))

  (add-hook 'embark-collect-mode-hook #'+embark-collect-hook)

  :bind
  (("M-a"                     . #'embark-act)
   ("M-d"                     . #'embark-dwim)
   ("C-h B"                   . #'embark-bindings)
   ;; alternative for `describe-bindings'
   ([remap describe-bindings] . #'embark-bindings)
   (:map minibuffer-local-map
         (("C-o" . embark-export)
          (("C-c C-o" . embark-collect-live)
           ("C-c C-e" . diego/embark-export-write)))
         :map embark-collect-mode-map
         (("a" . embark-act)
          ("E" . embark-export)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))

;;*** marginalia.el
;; Provides annotations to completion candidates.
(use-package marginalia
  :init
  (marginalia-mode 1)
  :config
  (setq marginalia-truncate-width 120)
  (setq marginalia-field-width 120))

;;*** orderless.el
;; Orderless is a completion-style to allow convenient filters.
(use-package orderless
  :init
  ;; The =basic= completion style is specified as fallback in addition to =orderless= in order to
  ;; ensure that completion commands which rely on dynamic completion tables,
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  ;; Enable `partial-completion' for files to allow path expansion.
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  (setq completion-category-overrides nil)
  (setq completions-format 'one-column)
  (setq completions-detailed t)
  :config

  ;; adapted from https://github.com/minad/consult/wiki#orderless-style-dispatchers-ensure-that-the--regexp-works-with-consult-buffer
  (defvar +orderless-dispatch-alist
    '((?! . orderless-without-literal)
      (?, . orderless-initialism)
      (?= . orderless-literal)))

  ;; Recognizes the following patterns:
  ;; * =literal literal=
  ;; * ,initialism initialism,
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  (setq orderless-component-separator #'orderless-escapable-split-on-space) ;; allow escaping space with backslash
  (setq orderless-style-dispatchers '(+orderless-dispatch)))

;;** Key bindings hints (which-key.el)
;; [[https://github.com/justbur/emacs-which-key][which-key.el]] is a minor mode for Emacs that displays the key bindings following your currently
;; entered incomplete command (a prefix) in a popup.
;; Special SPC, TAB, etc., Single Character a-z,Modifier C-, M-, Other same as default, except single
;; characters are sorted alphabetically
(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha) ;
  (setq which-key-sort-uppercase-first nil) ; I prefer to have lowercase first when there is for example a k and K
  (setq which-key-max-display-columns nil)
  (setq which-key-min-display-lines 10)
  (setq which-key-side-window-slot -10); A negative value means use a slot preceding (that is, above or on the left of) the middle slot.
  (setq which-key-idle-delay 0.3)
  :config
  (which-key-mode 1))

(provide 'diego-completion)
