;;* Workspaces/tabs (tab-bar.el)

(use-package tab-bar
  :after  modus-themes
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-auto-width nil) ; I don't like the width of the tabs

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

  (defface diego-modus-themes-mark-sel '((t :inherit modus-themes-mark-sel :bold nil)) "diego-modus-themes-mark-sel")
  (defface diego-modus-themes-mark-alt '((t :inherit modus-themes-mark-alt :bold nil)) "diego-modus-themes-mark-alt")
  (defface diego-modus-themes-mark-del '((t :inherit modus-themes-mark-del :bold nil)) "diego-modus-themes-mark-del")

  (defun diego-tab-format-empire ()
    "May the force be with you"
    `((global menu-item ,(propertize (concat "      " ) 'face 'diego-modus-themes-mark-del) ignore)))

  (defun diego-tab-format-vc ()
    "Format VC status for the tab bar."
    `((global menu-item ,(propertize (concat " " vc-mode "  ") 'face 'diego-modus-themes-mark-alt) ignore)))

  (defun diego-tab-format-buffer-id ()  ;
    "Buffer true name for files or just the buffer name."
    (ignore-errors
      `((global menu-item ,(propertize
                            (concat (if (and buffer-file-truename (not (file-remote-p buffer-file-truename)) (diego/current-project-root))
                                        (concat "File: " (all-the-icons-icon-for-buffer) " "
                                                (if (string-prefix-p (diego/current-project-root) buffer-file-truename)
                                                    (car (split-string  buffer-file-truename (diego/current-project-root) t nil))
                                                  buffer-file-truename))
                                      (concat "Buffer: " (all-the-icons-icon-for-buffer) " " (buffer-name))))
                            'face 'diego-modus-themes-mark-sel) ignore))))

  (defun diego-tab-format-keycast ()
    "Format `mode-line-modified' for the tab bar."
    `((global menu-item ,(format-mode-line keycast-mode-line) ignore)))

  (defun diego-tab-format-line-break ()
    "Format a line break to simulate another row for the tab bar.
It needs an space before to stop any colour to follow at the end of the row."
    `((global menu-item " \n" ignore)))

  (setq tab-bar-format
        '(mode-line-front-space
          diego-tab-format-empire

          ;; prot-tab-format-modified
          prot-tab-format-evil
          tab-bar-format-tabs ;; tab-bar-format-tabs-groups ; remove as it duplicates the tabs
          ;; diego-tab-format-empire
          diego-tab-format-line-break
          diego-tab-format-vc
          diego-tab-format-buffer-id
          ;; diego-tab-format-keycast
          tab-bar-format-align-right
          tab-bar-format-global))

  (global-tab-line-mode 1)
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

(provide 'diego-tab-bar)
