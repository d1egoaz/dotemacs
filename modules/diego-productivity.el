;;* Productivity

;;** keyfreq.el
(use-package keyfreq
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          backward-char
          evil-backward-char
          evil-forward-char
          evil-forward-word-begin
          evil-mouse-drag-region
          evil-next-line
          evil-next-visual-line
          evil-normal-state
          evil-previous-line
          forward-char
          ignore
          lsp-ui-doc--handle-mouse-movement
          mouse-set-point
          mwheel-scroll
          next-line
          previous-line
          vertico-exit
          vertico-next
          pixel-scroll-precision
          org-self-insert-command
          vterm--self-insert
          ))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;** ediff.el
(use-package ediff
  :straight (:type built-in)
  :config
  (setq ediff-split-window-function #'split-window-horizontally)
  ;; stop creating a new frame for navigating changes
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))

;;** diff-hl.el
(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  :hook ((prog-mode-hook       . diff-hl-mode)
         (gfm-mode-hook        . diff-hl-mode)
         (org-mode-hook        . diff-hl-mode)))

;;** Syntax checking (flycheck.el)
(use-package flycheck
  :config
  (setq-default flycheck-yamllintrc "~/.config/yamllint/config")
  (global-flycheck-mode 1))

;;** Snippets (yasnippet.el)
;;*** yasnippet.el
;; https://github.com/joaotavora/yasnippet
;; YASnippet is a template system for Emacs. It allows you to type an abbreviation and automatically expand it into function templates.
(use-package yasnippet
  :config
  (setq yas-verbosity 2)
  (setq yas-snippet-dirs `(,(expand-file-name "snippets/" user-emacs-directory)))
  (yas-global-mode 1))

;;*** Snippets collection
;; https://github.com/hlissner/doom-snippets
;; needs files * to download the snippets directories
(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*"))
  :config
  (yas-global-mode 1))

;;** flyspell.el
;; `z=` to correct word.
(use-package flyspell
  :after org
  :config
  (setq ispell-program-name "aspell")
  :hook ((prog-mode-hook       . flyspell-prog-mode)
         (gfm-mode-hook        . flyspell-prog-mode)
         (text-mode-hook       . flyspell-mode)
         (git-commit-mode-hook . flyspell-mode)
         (org-mode-hook        . flyspell-mode)))

(use-package flyspell-correct
  :after flyspell
  :bind (([remap ispell-word] . #'flyspell-correct-at-point))
  :config
  ;; provides save, skip
  (setq flyspell-correct-interface #'flyspell-correct-dummy))

;;** Define word
;; Use directly this server instead of trying localhost.
;; dict.org uses Webster 1913 dictionary.
(use-package dictionary
  :straight (:type built-in)
  :config
  (setq dictionary-server "dict.org"))

(use-package define-word)

;;** buff-move.el
;; Package `buffer-move' provides simple commands to swap Emacs windows: `buf-move-up',
;; `buf-move-down', `buf-move-left', `buf-move-right'.
(use-package buffer-move)

;;** ripgrep.el
(use-package ripgrep)

;;** iedit.el
(use-package iedit
  :config
  (add-to-list 'warning-suppress-types '(iedit)) ; to avoid warn edit default key %S is occupied by %s

   ;;;###autoload
  (defun diego/iedit-scoped (orig-fn)
    "Call `iedit-mode' with function-local scope, or global scope if called with a universal prefix."
    (interactive)
    (pcase-exhaustive current-prefix-arg
      ('nil (funcall orig-fn '(0)))
      ('(4) (funcall orig-fn))))

  (advice-add #'iedit-mode :around #'diego/iedit-scoped))

;;** which-function-mode.el
(use-package which-func
  :straight (:type built-in)
  :config
  ;; Show the current function name in the header line
  (setq-default header-line-format
                '((which-function-mode ("" which-func-format " "))))
;;  (:eval (breadcrumb-project-crumbs))))

  ;; We remove Which Function Mode from the mode line, because it's mostly
  ;; invisible here anyway.
  (setq mode-line-misc-info (assq-delete-all 'which-function-mode mode-line-misc-info))

  (which-function-mode -1))

;;** blamer.el
(use-package breadcrumb
  :straight (:type git :host github :repo "joaotavora/breadcrumb")
  :config
  (breadcrumb-mode 1))

;;** bookmarks.el
(use-package bookmark
  :after org
  :straight (:type built-in)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-set-fringe-mark t) ; Emacs28
  (setq bookmark-save-flag 1)

  (setq bookmark-default-file "~/Documents/deft/bookmarks.el")
  (setq bookmark-watch-bookmark-file 'silent)

  (defun diego--bookmark-url-handler (bookmark)
    (browse-url (bookmark-prop-get bookmark 'url)))

  (defun diego/bookmark-set-url (url name)
    (interactive "sBookmark URL: \nsBookmark name: ")
    (if (assoc name bookmark-alist)
        (user-error "%s is already bookmarked" name)
      (push `(,name . ((handler . ,#'diego--bookmark-url-handler)(url . ,url)(filename . ,url)))
            bookmark-alist))))

;;** blamer.el
(use-package blamer
  :straight (:type git :host github :repo "artawower/blamer.el")
  :init
  (setq blamer-idle-time 1.0)
  (setq blamer-min-offset 60))
;; :config
;; (global-blamer-mode 1))

;;** string-inflection.el
(use-package string-inflection)

;;** undo-fu.el
(use-package undo-fu
  :config
  (setq undo-limit 8000000           ; 8mb (default is 160kb)
        undo-strong-limit 8000000   ; 8mb   (default is 240kb)
        undo-outer-limit 48000000))  ; 48mb  (default is 24mb)

(use-package undo-fu-session
  :after undo-fu
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-incompatible-major-modes '(vterm-mode kubel-mode))
  (global-undo-fu-session-mode 1)
  :hook ((prog-mode-hook       . undo-fu-session-mode)
         (gfm-mode-hook        . undo-fu-session-mode)
         (org-mode-hook        . undo-fu-session-mode)
         (text-mode-hook       . undo-fu-session-mode)))

(use-package c3po
  :straight (:host github :repo "d1egoaz/c3po.el" :branch "d1egoaz_test-composition")
  :bind ("<f7>" . #'c3po-pop-results-buffer)
  :config

  (setq c3po-model "gpt-3.5-turbo")
  (setq c3po-temperature 0.2) ;; 0.2 will make it more focused and deterministic.

  ;; (setq c3po-api-key (diego/auth-source-get-password "api.openai.com" "personal"))
  (setq c3po-api-key (diego/auth-source-get-password "api.openai.com" "personal"))
  (c3po-add-new-droid '(synonymizer . (:system-prompt
                                       "
I want you to act as a synonyms provider.
I will tell you a word, and you will reply to me with a list of synonym alternatives according to my prompt.
Provide a list of 5 synonyms per prompt, 3 short examples, and a list of 5 antonyms.
You will only reply the words list, and nothing else, please use this template:
**Synonyms:**
-

**Examples:**
-

**Antonyms:**
-
"))))

(provide 'diego-productivity)
