;;* Development

;;** Language Server Support (LSP)
;; https://emacs-lsp.github.io/lsp-mode/page/main-features/
;; https://github.com/emacs-lsp/lsp-treemacs
;; - lsp-treemacs-symbols
;; - lsp-treemacs-errors-list
;; - lsp-treemacs-references/lsp-treemacs-implementations
;; - lsp-treemacs-call-hierarchy
;; - consult-lsp-diagnostics
;; - consult-lsp-symbols
;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package lsp-mode
  :after (corfu orderless)
  :commands lsp-deferred
  :init
  (setq lsp-keymap-prefix "C-l")
  ;; corfu + orderless
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  :config
  (setq lsp-completion-provider :none) ;; we use Corfu!
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'")

  ;; Project errors on modeline
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-diagnostics-scope :workspace)

  ;; For a UI feedback on headerline of the document
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project))

  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)

  (evil-add-command-properties #'lsp-find-definition :jump t)
  (evil-add-command-properties #'lsp-goto-type-definition :jump t)
  (advice-add 'lsp-goto-type-definition :before #'evil-set-jump)
  (advice-add 'lsp-find-definition :before #'evil-set-jump)

  ;;test
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05)

  (lsp-enable-which-key-integration t)

  (defun diego--lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  :hook
  ((lsp-completion-mode-hook . my/lsp-mode-setup-completion)
   (lsp-mode-hook . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  ;; Show informations of the symbols on the current line
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions t)

  ;; Add peek feature
  (setq lsp-ui-peek-enable t)
  ;; lsp-ui-peek-show-directory show the directory of files

  ;; Show object documentation at point in a child frame.
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'top)

  ;; imenu
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-imenu-auto-refresh t)

  :hook ((lsp-mode-hook . lsp-ui-mode)))

;;** eglot
(use-package eglot
  :commands eglot eglot-ensure
  :config
  (setq eglot-autoshutdown t))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . #'consult-eglot-symbols)))

;;** Go
;;*** Get latest gopls
;; sh
;; go install golang.org/x/tools/gopls@latest #
;;*** get latest goimports
;; sh
;; go install golang.org/x/tools/cmd/goimports@latest

;;*** configure local variables for a project

;; Call =add-dir-local-variable=, result:
;; #+begin_example elisp file: .dir-locals.el
;; ;;; Directory Local Variables
;; ;;; For more information see (info "(emacs) Directory Variables")
;; ((go-mode . ((lsp-go-goimports-local . "github.com/Shopify/cloudbuddies")
;;              (gofmt-args . ("-local" "github.com/Shopify/cloudbuddies")))))
;; #+end_example
;;*** go-mode.el
(use-package go-mode
  :general
  (general-nvmap
    :keymaps 'go-mode-map
    :prefix ","
    "a" #'go-tag-add
    "i" #'go-goto-imports
    "." #'godoc-at-point
    "t" '(:ignore t :which-key "test")
    ;; "tt" #'diego/go-run-test-current-function
    "tt" #'go-test-current-test
    "tf" #'go-test-current-file
    "tg" #'go-gen-test-exported)
  :config
  ;; (setq-default lsp-go-goimports-local "github.com/Shopify/")
  (setq godef-command "godef") ; original godef
  ;; (setq godef-command "go doc") ; original godef
  (setq gofmt-command "goimports") ; original gofmt
  (setq gofmt-args nil)
  ;; (setq gofmt-args '("-local" "github.com/Shopify/"))

  (defun outline-go-mode-hook ()
    (set (make-local-variable 'outline-regexp) "\\(func \\|\\(.*struct {\\)\\|\\(type \\)\\)"))

  :hook ((go-mode-hook . outline-go-mode-hook)
         (go-mode-hook . lsp-deferred)
         (go-mode-hook . diego--lsp-go-install-save-hooks)))
;; (go-mode-hook .  eglot-ensure)))

;;*** ob-go.el
;; Org-Babel support for evaluating go code.
;; https://github.com/pope/ob-go
(use-package ob-go
  :after (go-mode org)
  :straight (ob-go :type git :host github :repo "pope/ob-go"))

;;*** flycheck-golangci
(use-package flycheck-golangci-lint
  ;; :after (flycheck go-mode lsp)
  :config
  ;; (setq flycheck-golangci-lint-config "/Users/diegoalvarez/code/go/.golangci.yml")
  (setq flycheck-golangci-lint-fast t)
  (setq flycheck-golangci-lint-tests t)
  (setq flycheck-golangci-lint-enable-linters '(
                                                ;; default
                                                "deadcode" "errcheck" "gosimple" "govet" "ineffassign"
                                                "staticcheck" "structcheck" "typecheck" "unused" "varcheck"
                                                ;; extras
                                                "errname" "errorlint" "exhaustive" "exportloopref" "gocritic" "goconst"
                                                "gocritic" "godot"  "gofmt" "goimports" "gosec" "govet" "ifshort"
                                                "makezero" "nestif" "nilerr" "noctx" "paralleltest" "prealloc" "predeclared"
                                                "revive"  "stylecheck" "testpackage" "unconvert" "unparam"
                                                "varnamelen" "wastedassign" "whitespace" "wsl"
                                                ;; experiment
                                                "wrapcheck"
                                                ;;"goerr113"
                                                ))

  (defun diego--setup-golangci-lint ()
    (flycheck-golangci-lint-setup)
    (push 'golangci-lint flycheck-checkers))
  ;; (flycheck-add-next-checker 'lsp-ui 'golangci-lint))

  :hook ((go-mode-hook . diego--setup-golangci-lint)))

;;*** go-gen-test
(use-package go-gen-test)

;;*** gotest.el
(use-package gotest
  :after go-mode
  :config
  (dolist (elt go-test-compilation-error-regexp-alist-alist)
    (add-to-list 'compilation-error-regexp-alist-alist elt))

  (defun prepend-go-compilation-regexps ()
    (dolist (elt (reverse go-test-compilation-error-regexp-alist))
      (add-to-list 'compilation-error-regexp-alist elt t)))

  (add-hook 'go-mode-hook 'prepend-go-compilation-regexps))

;;*** go-impl
;; go-impl generates method stubs for implementing an interface.
(use-package go-impl)

;;** Rust
;; https://github.com/brotzeit/rustic
;; #+begin_example
;; rustup component add rust-src
;; rustup component add rustfmt
;; cargo install cargo-script # to make it work in org mode babel
;; rustup toolchain install nightly # to use expand
;; cargo install cargo-expand
;; #+end_example
;; https://github.com/brotzeit/rustic#eglot
(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :general
  (general-nvmap
    :keymaps 'rust-mode-map
    :prefix ","
    "b" #'rustic-cargo-build
    "c" '(:ignore t :which-key "cargo")
    "ca" #'rustic-cargo-add
    "cc" #'rustic-cargo-clippy
    "ci" #'rustic-cargo-add-missing-dependencies
    "cu" #'rustic-cargo-upgrade
    "co" #'rustic-cargo-outdated
    "d"  #'rustic-doc-search
    "e" #'rustic-cargo-expand
    "f" #'rustic-cargo-fmt
    "t" '(:ignore t :which-key "test")
    ;; "tt" #'diego/go-run-test-current-function
    "tt" #'rustic-cargo-current-test
    "tf" #'rustic-cargo-test
    "T"  #'lsp-rust-analyzer-related-tests
    "r" #'rustic-cargo-run)

  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  (defun diego--rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))

  :hook ((rustic-mode-hook . lsp-deferred)
         (rustic-mode-hook . diego--lsp-go-install-save-hooks)
         (rustic-mode-hook . diego--rustic-mode-auto-save-hook)))
;;(rust-mode-hook .  eglot-ensure)

;;** Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :general
  (general-nvmap
    :keymaps 'gfm-mode-map
    :prefix ","
    "p" #'markdown-preview
    "P" #'markdown-live-preview-mode
    "s" #'markdown-insert-gfm-code-block
    "l" #'markdown-insert-link)
  :mode (("\\.md\\'"       . #'gfm-mode)
         ("\\.markdown\\'" . #'gfm-mode)
         ("readme\\.txt\\'" . markdown-mode)
         ("README\\.txt\\'" . markdown-mode))
  :config
  ;; Display remote images
  (setq markdown-display-remote-images t)
  ;; Enable fontification for code blocks
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-header-scaling t)
  (setq markdown-gfm-uppercase-checkbox t)
  (setq markdown-make-gfm-checkboxes-buttons t)

  (setq markdown-italic-underscore t)
  (setq markdown-gfm-additional-languages '("sh" "yaml" "yml"))
  ;; (setq markdown-gfm-additional-languages nil)

  ;; adds highlightjs syntax coloring.
  ;; based on doom config
  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown-dark.min.css"
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github-dark.min.css"))

  (setq markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                ;; follows this to highlight source code blocks https://github.com/highlightjs/highlight.js#using-custom-html
                "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))

  (setq markdown-command "pandoc -f gfm --highlight-style pygments --toc -t html --metadata pagetitle=MarkdownPreview"))


;;** Misc
(use-package dockerfile-mode)
(use-package graphql-mode :mode "\\.g\\(?:raph\\)?ql$")
(use-package json-mode)
(use-package nix-mode)
(use-package terraform-mode)
(use-package web-mode
  :mode (("\\.html.erb\\'"       . web-mode)))

;;** yaml
(use-package yaml-mode
  :mode (("\\.yml\\'"       . #'yaml-mode)
         ("\\.yaml\\'"      . #'yaml-mode)
         ("\\.yml.erb\\'"      . #'yaml-mode)
         ("\\.yaml.lock\\'" . #'yaml-mode)))

(use-package yaml
  :straight (:host github :repo "zkry/yaml.el"))

(use-package yaml-pro
  :after yaml
  :straight (:host github :repo "zkry/yaml-pro")
  :general
  (general-nvmap
    :keymaps 'yaml-mode-map
    :prefix ","
    "j" #'yaml-pro-consult-jump
    "ff" #'yaml-pro-fold-at-point
    "fu" #'yaml-pro-unfold-at-point
    ">" #'yaml-pro-indent-subtree
    "<" #'yaml-pro-unindent-subtree
    "K" #'yaml-pro-move-subtree-up
    "J" #'yaml-pro-move-subtree-down
    "'" #'yaml-pro-edit-scalar)
  :hook
  (yaml-mode-hook  . yaml-pro-mode))

;;** Kubernetes

;;*** kubel.el
(use-package kubel
  ;; :straight (kubel :host github :repo "d1egoaz/kubel" :branch "diego/multiple-kubel-buffers")
  :straight (kubel :host github :repo "d1egoaz/kubel" :branch "test")
  :config
  (evil-define-key 'normal 'kubel-yaml-editing-mode "q" #'kill-current-buffer)

  ;; https://github.com/abrochard/kubel/issues/53 https://github.com/abrochard/kubel/pull/44
  ;; (setq kubel-use-namespace-list 'on)
  ;; list namespaces automatically
  (setq kubel-use-namespace-list 'on) ; I'm now using my own branch
  (setq-default kubel-namespace "cloudbuddies")

  (defun diego--kubel-hook ()
    (require 'f)
    (f-mkdir "/tmp/kubel") ; unset default directory
    (f-touch "/tmp/kubel/.project")
    (add-to-list 'savehist-additional-variables 'kubel--context-list-cached)
    (add-to-list 'savehist-additional-variables 'kubel--namespace-list-cached)
    (add-to-list 'savehist-additional-variables 'kubel--kubernetes-resources-list-cached))
  :hook ((kubel-mode-hook . diego--kubel-hook)))

(use-package kubel-evil
  ;; :load-path "/Users/diegoalvarez/code/oss/kubel2"
  :load-path "/Users/diegoalvarez/.emacs.d/straight/repos/kubel/kubel-evil.el"
  :after (kubel evil))

;;** hideshow.el
;; Hideshow mode is a buffer-local minor mode that allows you to selectively
;; display portions of a program, which are referred to as blocks.

;;   hs-hide-block                      C-c @ C-h
;;   hs-show-block                      C-c @ C-s
;;   hs-hide-all                        C-c @ C-M-h
;;   hs-show-all                        C-c @ C-M-s
;;   hs-hide-level                      C-c @ C-l
;;   hs-toggle-hiding                   C-c @ C-c
;;   hs-toggle-hiding                   [(shift mouse-2)]
;;   hs-hide-initial-comment-block
(use-package hideshow
  :straight (:type built-in)
  :config
  (setq hs-hide-comments-when-hiding-all nil) ; dont' hide the comments too when you do a 'hs-hide-all'

  ;; Global hide/show toggle
  (defvar diego--my-hs-hide nil "Current state of hideshow for toggling all.")
  (defun diego/toggle-hideshow-all ()
    "Toggle hideshow all."
    (interactive)
    (setq diego--my-hs-hide (not diego--my-hs-hide))
    (if diego--my-hs-hide
        (hs-hide-all)
      (hs-show-all)))

  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "{" "[")) ; Block start
                 ,(rx (or "}" "]" "end"))                  ; Block end
                 ,(rx (or "#" "=begin"))                   ; Comment start
                 ruby-forward-sexp nil))
  :hook
  (go-mode-hook . hs-minor-mode))

;;** Compilation mode
;;*** Basic configuration
(use-package compile
  :straight (:type built-in)
  :config
  (setq comint-buffer-maximum-size 8192); Increase comint buffer size.
  (setq comint-input-ignoredups t)
  (setq comint-scroll-to-bottom-on-input t) ; always insert at the bottom
  (setq comint-scroll-to-bottom-on-output nil) ; always add output at the bottom

  (setq compilation-scroll-output 'first-error)
  ;; (setq compilation-scroll-output t)
  ;; (setq compilation-auto-jump-to-first-error t)
  (setq compilation-auto-jump-to-first-error 'if-location-known)
  (setq compilation-always-kill t) ; kill old compile processes before starting the new one
  (setq compilation-ask-about-save nil)  ; save all buffers on `compile'


  (defun compilation-add-separator ()
    "Insert separator in read-only buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (insert "\n---------------------------------\n\n")
      (point-max)
      (comint-set-process-mark)))

  ;; from https://www.reddit.com/r/emacs/comments/wwdpju/comment/ilotsc5/?utm_source=share&utm_medium=web2x&context=3
  (defun meain/compilation-colorcode (_buffer string)
    "Change background color of compilation `_BUFFER' to red on failure."
    (unless (string-prefix-p "finished" string) ; Having color for success was distracting
      (face-remap-add-relative 'default 'lin-red)))
  (add-to-list 'compilation-finish-functions 'meain/compilation-colorcode)

  :bind (:map compilation-mode-map
              ("C-c -" . compilation-add-separator)
              ("-" . compilation-add-separator)
              :map comint-mode-map
              ("C-c -" . compilation-add-separator)))

;;** restclient.el
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :general
  (general-nvmap
    :keymaps 'restclient-mode-map
    :prefix ","
    "e"  #'restclient-http-send-current
    "E"  #'restclient-http-send-current-raw
    "c"  #'restclient-copy-curl-command))

;;** tree-sitter
(use-package tree-sitter
  :straight (tree-sitter :host github :repo "emacs-tree-sitter/elisp-tree-sitter")
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs :host github :repo "emacs-tree-sitter/tree-sitter-langs")
  :after tree-sitter)

;; #+begin_example elisp
;; (defun configure-imenu-Custom ()
;;   (setq imenu-generic-expression
;;         '(("Faces" "^\\(?:Show\\|Hide\\) \\(.*\\) face: \\[sample\\]" 1)
;;           ("Variables" "^\\(?:Show Value\\|Hide\\) \\([^:\n]*\\)" 1))))

;; (add-hook 'Custom-mode-hook #'configure-imenu-Custom)

(provide 'diego-programming)
