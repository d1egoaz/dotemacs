;; -*- lexical-binding: t; -*-
;;* Misc

;;** helpful.el
;; [[https://github.com/Wilfred/helpful][helpful.el]] is an alternative to the built-in Emacs help that provides much more contextual information.
(use-package helpful
  :bind
  (([remap describe-function] . #'helpful-callable)
   ([remap describe-variable] . #'helpful-variable)
   ([remap describe-symbol] . #'helpful-symbol)
   ([remap describe-key] . #'helpful-key))
  :general (general-nmap :keymaps 'helpful-mode-map "q" #'kill-buffer-and-window)
  :config (setq help-window-select t))


(use-package emacs
  :straight (:type built-in)
  :config (add-hook 'help-fns-describe-function-functions #'shortdoc-help-fns-examples-function))

;;** all-the-icons.el
;; To have some icons available in doom mode line.
(use-package all-the-icons
  :config (setq all-the-icons-scale-factor 0.9))

(use-package all-the-icons-dired
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode 1))

;;** World Clock
;; Tz zones: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones.
;; =format-time-string= for time format. ISO 8601 format =%FT%T%z=.
(use-package time
  :general (general-nmap :keymaps 'world-clock-mode-map "q" #'kill-buffer-and-window)
  :config
  (setq zoneinfo-style-world-list
        '(("etc/UTC" "UTC")
          ("America/Vancouver" "PT")
          ("America/New_York" "ET")
          ("America/Bogota" "Bogota")
          ("America/Toronto" "Toronto")))
  (setq world-clock-time-format "%A %d %B %R (%Z %z) %FT%T%z")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  :hook (after-init-hook . display-time-mode))

;;** olivetti.el
(use-package olivetti
  :config
  (setq olivetti-minimum-body-width 200)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode diego/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters."
    :init-value nil
    :global
    nil
    (if diego/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil))))

;;** RSS (elfeed.el)
;; The best RSS reader.
(use-package elfeed
  :after olivetti
  :general
  ;; format-next-line: off
  (general-nmap
    :keymaps 'elfeed-search-mode-map
    "c" #'elfeed-search-clear-filter
    "s" #'elfeed-search-live-filter
    "r" #'elfeed-search-untag-all-unread
    "," #'diego/elfeed-filter)
  (general-nmap :keymaps 'elfeed-show-mode-map "C-n" #'elfeed-show-next "C-p" #'elfeed-show-prev)
  :commands elfeed
  :config
  (setq elfeed-search-date-format '("%a %b-%d" 10 :left))
  (setq elfeed-search-filter "@2-week-ago +unread")
  (setq elfeed-search-title-max-width 120)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-sort-order 'ascending)

  (defun diego-elfeed-search-print-entry--default (entry)
  "Print ENTRY to the buffer."
  (let* (;;(date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         ;; (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         ;; (tags-str (mapconcat
         ;;            (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
         ;;            tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    ;; (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    ;; (when tags (insert "(" tags-str ")"))
    ))

  (setq elfeed-search-print-entry-function #'diego-elfeed-search-print-entry--default)
  (set-face-attribute 'elfeed-search-unread-title-face nil :weight 'normal)
  ;; (set-face-attribute 'elfeed-search-unread-title-face nil
  ;;                     :background "#001904"
  ;;                     :foreground "#b8e2b8"
  ;;                     :weight 'normal)

  (defun diego/elfeed-filter-do ()
    (interactive)
    (let ((tags (mapconcat 'identity (transient-args 'diego/elfeed-filter) " ")))
      (elfeed-search-clear-filter)
      (elfeed-search-set-filter (format "@2-weeks-ago +unread %s" tags))
      (goto-char (point-min))))

  (transient-define-prefix
    diego/elfeed-filter ()
    [["Arguments"
      ("a" "apple" "+apple")
      ("c" "Tech Crunch" "+techcrunch")
      ("e" "emacs" "+emacs")
      ("h" "Hacker News" "+hnews")
      ("l" "linux" "+linux")
      ("t" "top" "+top")
      ("s" "sre" "+sre")
      ("v" "verge" "+theverge")
      ("w" "aws" "+aws")]
     ["Reddit" ("p" "r/Programming" "+programming")]
     ["Actions" ("f" "apply" diego/elfeed-filter-do) ("u" "update" elfeed-update)]])

  :hook (
         (elfeed-search-mode-hook . diego/olivetti-mode)
         (elfeed-search-mode-hook . mixed-pitch-mode)
         (elfeed-show-mode-hook . diego/olivetti-mode)
         ))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list "~/Documents/deft/elfeed.org"))
  (elfeed-org))

;;** emacs-everywhere.el
;; By default, emacs-everywhere-insert-selection is a hook in
;; emacs-everywhere-init-hooks, and will insert the last text selection into your
;; new buffer. To clear this, type DEL or C-SPC before anything else.
;; Once you’ve finished and want to insert the text into the window you triggered
;; Emacs Everywhere from, just press C-c C-c.
(use-package emacs-everywhere
  :straight (:host github :repo "tecosaur/emacs-everywhere")
  :general
  ;; format-next-line: off
  (general-nmap
    :keymaps 'emacs-everywhere-mode-map
    "," #'diego/emacs-everywhere-filter
    "C-c C-c" #'emacs-everywhere--finish-or-ctrl-c-ctrl-c)
  :bind ("s-g" . #'c3po-transient-tools)

  :config
  (setq emacs-everywhere-clipboard-sleep-delay 0.1)
  (add-hook 'emacs-everywhere-mode-hook #'evil-normal-state)

  (defun diego/emacs-everywhere-set-frame-position ()
    "Set the size and position of the emacs-everywhere frame."
    (let ((frame (selected-frame)))
      (set-frame-size frame 210 55)
      (set-frame-position frame 100 10)))

  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-position)
  (add-hook 'emacs-everywhere-init-hooks #'diego/emacs-everywhere-set-frame-position)

  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-init-spell-check)
  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-remove-trailing-whitespace)
  (add-hook 'emacs-everywhere-init-hooks #'mark-whole-buffer t) ; t add the hook at the beginning

  (setq emacs-everywhere-final-hooks nil)

  (setq emacs-everywhere-major-mode-function #'gfm-mode)

  (defun diego--custom-c3po-assistant ()
    (interactive)
    (let ((current-prefix-arg '(8)))
      (call-interactively 'c3po-assistant-new-chat-replace-region)))

  (defun diego--tmp-everywhere ()
    (mark-whole-buffer)
    (c3po-assistant-new-chat-replace-region))

  (defun emacs-everywhere-grammar-checker ()
    (interactive)
    (emacs-everywhere)
    (run-with-timer 3 nil #'diego--tmp-everywhere))

  (transient-define-prefix
    diego/emacs-everywhere-filter ()
    [["Actions"
      ("g" "correct grammar" c3po-grammar-checker-new-chat-replace-region)
      ("a" "assistant" diego--custom-c3po-assistant)
      ("w" "rewrite" c3po-rewriter-new-chat-replace-region)]]))

;; to signal emacs-everywhere to use org-gfm-export-to-markdown
;; currently not used as I'm always using gfm-mode
(use-package ox-gfm
  :after org
  :config (require 'ox-gfm nil t))

(use-package show-font
  :straight (:host github :repo "protesilaos/show-font"))


(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(provide 'diego-misc)
