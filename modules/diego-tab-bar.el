;;* Workspaces/tabs (tab-bar.el)

(use-package tab-bar
  :after modus-themes
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-auto-width nil) ; I don't like the width of the tabs

  ;; for fonts to show up use: all-the-icons-install-fonts first
  (setq tab-bar-new-tab-choice nil)
  (setq tab-bar-separator "  ")
  (setq tab-bar-show t)
  (setq tab-bar-tab-hints nil) ; don't show numbers
  (setq tab-bar-tab-pre-close-functions '(diego--close-workspace))

  (setq global-mode-string '("  " display-time-string))

  (defun prot-tab-format-evil ()
    "Format `evil-mode-line-tag for the tab bar."
    `((global menu-item ,evil-mode-line-tag ignore)))

  (defun prot-tab-format-modified ()
    "Format `mode-line-modified' for the tab bar."
    `((global menu-item ,(string-trim-right (format-mode-line mode-line-modified)) ignore)))

  ;; modify theme and remove (beginning-of-line)d
  (defface diego-modus-themes-mark-sel '((t :foreground "#00d3d0" :background "#004065" :bold nil)) "diego-modus-themes-mark-sel")
  (defface diego-modus-themes-mark-alt '((t :foreground "#d0bc00" :background "#4a4000" :bold nil  )) "diego-modus-themes-mark-alt")
  (defface diego-modus-themes-mark-del '((t :foreground "#ff7f9f" :background "#620f2a" :bold nil)) "diego-modus-themes-mark-del")

  (defun diego-tab-format-empire ()
    "May the force be with you"
    `((global menu-item ,(propertize (concat "      " ) 'face 'diego-modus-themes-mark-del) ignore)))

  (defun diego-tab-format-vc ()
    "Format VC status for the tab bar."
    `((global menu-item ,(propertize (concat "" vc-mode " ") 'face 'diego-modus-themes-mark-alt) ignore)))

  (defun diego-tab-format-buffer-id ()  ;
    "Buffer true name for files or just the buffer name."
    (ignore-errors
      `((global menu-item ,(propertize
                            (concat (if (and buffer-file-truename (not (file-remote-p buffer-file-truename)) (diego/current-project-root))
                                        (concat "File: " (all-the-icons-icon-for-file buffer-file-truename :height 2 :v-adjust 0.1) " "
                                                (if (string-prefix-p (diego/current-project-root) buffer-file-truename)
                                                    (car (split-string  buffer-file-truename (diego/current-project-root) t nil))
                                                  buffer-file-truename))
                                      (concat "Buffer: " (all-the-icons-icon-for-buffer) " " (buffer-name))))
                            'face 'all-the-icons-dblue) ignore))))

  (defun diego-tab-format-keycast ()
    "Format `mode-line-modified' for the tab bar."
    `((global menu-item ,(format-mode-line keycast-mode-line) ignore)))

  (defun diego-tab-format-line-break ()
    "Format a line break to simulate another row for the tab bar.
It needs an space before to stop any colour to follow at the end of the row."
    `((global menu-item " \n" ignore)))

  (defun diego-tab-format-kubel-context ()
    "Show kubel context"
    `((global menu-item ,(propertize (concat " :" (when (boundp 'kubel-context) kubel-context)) 'face 'diego-modus-themes-mark-del) ignore)))

  (defun diego-tab-format-kubel-namespace ()
    "Show kubel namespace"
    `((global menu-item ,(propertize (concat " :" (when (boundp 'kubel-namespace) kubel-namespace)) 'face 'diego-modus-themes-mark-alt) ignore)))

  (defun diego-tab-format-kubel-resource ()
    "Show kubel namespace"
    `((global menu-item ,(propertize (concat " :" (when (boundp 'kubel-resource) kubel-resource)) 'face 'diego-modus-themes-mark-sel) ignore)))

  (setq tab-bar-format
        '(mode-line-front-space
          diego-tab-format-empire

          ;; prot-tab-format-modified
          prot-tab-format-evil
          tab-bar-format-tabs ;; tab-bar-format-tabs-groups ; remove as it duplicates the tabs
          tab-bar-separator
          ;; diego-tab-format-empire
          diego-tab-format-line-break
          diego-tab-format-vc
          diego-tab-format-buffer-id
          diego-tab-format-kubel-context
          diego-tab-format-kubel-namespace
          diego-tab-format-kubel-resource
          ;; diego-tab-format-keycast
          tab-bar-format-align-right
          tab-bar-format-global))

  (global-tab-line-mode 1)
  (tab-bar-mode 1)
  (tab-bar-history-mode 1)

  (add-hook 'tab-bar-switch-functions #'diego-apply-current-buffer-display-rules))

(provide 'diego-tab-bar)
