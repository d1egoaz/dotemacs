;;* Search And Replace

;;** Jumping with (avy.el)
;; [[https://github.com/abo-abo/avy][avy]] is used to jump to visible text using chars.
(use-package avy
  :config
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq avy-style 'at-full))

;;** imenu-list.el
;; https://github.com/bmag/imenu-list
;; Emacs plugin to show the current buffer's imenu entries in a seperate buffer.
(use-package imenu-list
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t)
  (set-face-attribute 'imenu-list-entry-face nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-face-0 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-face-1 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-face-2 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-face-3 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-subalist-face-0 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-subalist-face-1 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-subalist-face-2 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-subalist-face-3 nil :height 0.7))

;;** Writable grep (wgrep.el)
;; With =wgrep= we can directly edit the results of a =grep= and save the
;; changes to all affected buffers.
;; To save all buffers that wgrep has changed, run M-x wgrep-save-all-buffers
;; I then press C-c C-c (wgrep-finish-edit).
;; consult-line -> embark-export to occur-mode buffer -> occur-edit-mode for editing of matches in buffer.
;; consult-grep -> embark-export to grep-mode buffer -> wgrep for editing of all matches.
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;;** Visual regex
;; Package `visual-regexp-steroids' allows `visual-regexp' to use regexp engines other than Emacs'; for
;; example, Python or Perl regexps.
(use-package visual-regexp
  :config
  (setq vr/default-replace-preview t))

(use-package visual-regexp-steroids
  :after visual-regexp
  :bind (([remap query-replace-regexp] . #'vr/query-replace)))

(provide 'diego-search-jump)
