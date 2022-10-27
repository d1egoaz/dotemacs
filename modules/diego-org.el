;;* Org Mode

;;** org.el
(use-package org
  :general
  ;; I prefer C-c C-c over C-c ' (more consistent)
  (:keymaps
   'org-src-mode-map
   "C-c C-c" #'org-edit-src-exit)
  (:keymaps
   'org-mode-map
   "C-j"  #'org-move-subtree-down
   "C-k"  #'org-move-subtree-up
   "M-h" nil
   "M-l" nil)
  ;; local leader
  (general-nvmap
    :keymaps 'org-mode-map
    :prefix ","
    "'" #'org-edit-special
    "e" #'org-export-dispatch
    "h" #'org-toggle-heading
    "i" #'org-toggle-item
    "q" #'org-set-tags-command
    "t" #'org-todo
    "x" #'org-toggle-checkbox
    "a"  '(:ignore t :which-key "attachments")
    "aa" #'org-attach
    "ar" #'org-attach-reveal
    "au" #'org-attach-url
    "ac" #'org-download-screenshot
    "l"  '(:ignore t :which-key "link")
    "li" #'org-id-store-link
    "ll" 'org-insert-link
    "ls" 'org-store-link
    "d"   '(:ignore t :which-key "date/deadline")
    "dd" #'org-deadline
    "ds" #'org-schedule
    "dt" #'org-time-stamp)
  :init
  (setq org-directory "~/Documents/deft")
  (setq org-agenda-files (list "~/Documents/deft/journal.org" "~/Documents/deft/gtd-inbox.org" "~/Documents/deft/gtd-personal.org" "~/Documents/deft/gtd-work.org" ))
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-agenda-deadline-faces
        '((1.001 . error)
          (1.0 . org-warning)
          (0.5 . org-upcoming-deadline)
          (0.0 . org-upcoming-distant-deadline)))
  (setq org-agenda-span 'month); or 'week
  (setq org-attach-id-dir (file-name-as-directory (concat (file-name-as-directory org-directory) "images")))
  (setq org-default-notes-file (concat (file-name-as-directory org-directory) "notes.org"))
  (setq org-refile-targets '(("~/Documents/deft/gtd-inbox.org" :maxlevel . 1) ("~/Documents/deft/gtd-personal.org" :level . 1) ("~/Documents/deft/gtd-work.org" :maxlevel . 2)))
  :config
  (setq org-blank-before-new-entry '((heading . always) (plain-list-item . nil)))
  (setq org-clock-out-remove-zero-time-clocks nil)
  ;; (setq org-cycle-emulate-tab 'white) ; allows to collapse the current outline (call org-cycle)
  (setq org-confirm-babel-evaluate nil)
  (setq org-edit-src-content-indentation 0) ; not need to waste space
  (setq org-adapt-indentation nil) ; emacs sometimes hangs when using evil to open line above/below
  (setq org-ellipsis "⌄ ")
  (setq org-fontify-quote-and-verse-blocks t)
  ;; (setq org-hide-leading-stars nil)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers nil)
  (setq org-insert-heading-respect-content nil) ; Insert Org headings at point, not after the current subtree
  (setq org-log-into-drawer t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'reorganize-frame)
  (setq org-startup-with-inline-images t)
  (setq org-todo-keywords '((sequence "TODO(t!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c!)" "IN-PROGRESS(i!)")))
  (setq org-capture-templates
        '(
          ;; example:
          ;;   "t"                               = key
          ;;   "Todo"                            = description
          ;;   entry                             = type
          ;;   (file+headline "file" "tasks")    = target
          ;;   ""                                = template
          ;;   :prepend t                        = properties
          ;; https://orgmode.org/manual/Template-expansion.html
          ("t" "Todo" entry (file+headline "~/Documents/deft/gtd-inbox.org" "Inbox")
           "* TODO %?\nCreated on on %U\n" :prepend t :empty-lines 1)
          ("l" "Link" entry (file+headline "~/Documents/deft/notes.org" "Links")
           "* %? %^L %^g \n%T" :prepend t)
          ("n" "Note" entry (file+headline "~/Documents/deft/notes.org" "Notes")
           "* %^{title}%^g\n%T\n\n%?" :prepend t)
          ("j" "Journal" entry (file+olp+datetree "~/Documents/deft/journal.org")
           "* %?\nSCHEDULED: <%(org-read-date nil nil \"today\")>" :clock-in t :clock-resume t)))

;;** Make org-capture start in insert mode
(add-hook 'org-capture-mode-hook #'evil-insert-state)

;;** Org-Babel
(org-babel-do-load-languages 'org-babel-load-languages
                             '(
                               (dot . t)
                               (shell . t)
                               (gnuplot . t)
                               (latex . t)
                               ))

;;** iMenu org depth
;; Increase the maximum level for Imenu access to Org headlines.
(setq org-imenu-depth 6))

;;** org-download.el
(use-package org-download
  :after org
  :commands org-download-screenshot
  :config
  (setq org-download-heading-lvl nil)
  (setq org-download-image-dir org-attach-directory)
  (setq org-download-image-html-width 500)
  (setq org-download-method 'attach)
  (setq org-download-screenshot-method "screencapture -i %s")
  (setq org-download-timestamp "_%Y%m%d_%H%M%S"))

;;** evil-org.el
;; https://github.com/hlissner/evil-org-mode
;; key	explanation
;; gh, gj, gk, gl	navigate between elements
;; vae	select an element
;; |------+----------------------+-------------------|
;; | key  | function             | explanation       |
;; |------+----------------------+-------------------|
;; | =gh= | org-element-up       | parent of element |
;; | =gj= | org-forward-element  | next element      |
;; | =gk= | org-backward-element | previous element  |
;; | =gl= | org-down-element     | first subelement  |
;; | =gH= | evil-org-top         | top-level heading |
;; |------+----------------------+-------------------|
;; all keybindings https://raw.githubusercontent.com/hlissner/evil-org-mode/master/doc/keythemes.org
(use-package evil-org
  :after (evil org)
  :straight (:host github :repo "hlissner/evil-org-mode")
  :config
  ;; enable bindings, remove `additional` as I want to use M-hjkl for different things
  (setq evil-org-key-theme '(heading insert navigation textobjects))
  (evil-org-set-key-theme)

  (general-nmap :keymaps 'evil-org-mode-map
    "M-h" nil
    "M-j" nil
    "M-k" nil
    "M-l" nil)

  (defun diego--org-set-key-theme ()
    (evil-org-set-key-theme))
  :hook ((org-mode-hook . evil-org-mode)
         (evil-org-mode-hook . diego--org-set-key-theme)))

;;** mermaid support
(use-package mermaid-mode)

(use-package ob-mermaid
  :after mermaid-mode)

;;** Create table of contents

;; To use, add a =:TOC:= tag to the headline.
;; Every time the file is saved, it'll be auto-updated with the current table of contents.

;; The table of contents heading may also be set with these tags:

;; - =:TOC_#:= Sets the maximum depth of the headlines in the table of
;;   contents to the number given, e.g. :TOC_3: for
;;   3 (default for plain :TOC: tag is 2).

;; - =:TOC_#_gh:= Sets the maximum depth as above and also uses
;;   GitHub-style anchors in the table of contents (the
;;   default).  The other supported style is :TOC_#_org:,
(use-package toc-org
  :after (org markdown-mode)
  :config
  (setq toc-org-max-depth 2)
  :hook ((org-mode-hook . toc-org-mode)
         (markdown-mode-hook . toc-org-mode)))

(provide 'diego-org)
