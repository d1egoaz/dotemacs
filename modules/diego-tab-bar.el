;;* Workspaces/tabs (tab-bar.el)

(use-package tab-bar
  :general
  (general-nmap
    "gt"  #'tab-next
    "gT"  #'tab-recent)
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-auto-width nil) ; I don't like the width of the tabs

  (setq tab-bar-new-tab-choice nil)
  (setq tab-bar-separator "|")
  (setq tab-bar-show t)
  (setq tab-bar-tab-hints nil) ; don't show numbers

  ;; close tab and project
  (defun diego--close-tab-for-project (tab _bool)
    (let ((name (alist-get 'name tab)))
      (if (string= "*kubel*" name)
          (kill-matching-buffers "\\*kubel" nil t)
        (when (project-current)
          (project-kill-buffers t)))
      (message "Project %s closed." name)))
  (setq tab-bar-tab-pre-close-functions '(diego--close-tab-for-project))

  (defun prot-tab-format-evil ()
    "Format `evil-mode-line-tag for the tab bar."
    `((global menu-item ,evil-mode-line-tag ignore)))

  (defun prot-tab-format-modified ()
    "Format `mode-line-modified' for the tab bar."
    `((global menu-item ,(string-trim-right (format-mode-line mode-line-modified)) ignore)))

  (defun diego--tab-insert-icon (name)
    `((global menu-item ,(propertize (all-the-icons-octicon name) 'face 'modus-themes-refine-yellow) ignore)))

  (defun diego-tab-format-vc ()
    "Format VC status for the tab bar."
    `((global menu-item ,(propertize (concat " " (all-the-icons-octicon "git-branch") vc-mode " ") 'face 'modus-themes-refine-yellow) ignore)))

  (defun diego-tab-format-buffer-id ()  ;
    "Buffer true name for files or just the buffer name."
    (ignore-errors
      `((global menu-item ,(propertize
                            (concat " " (all-the-icons-icon-for-buffer)
                                    (if (and buffer-file-truename (not (file-remote-p buffer-file-truename)) (diego/current-project-name))
                                        (concat " File: ["
                                                (if (string-prefix-p (diego/current-project-name) buffer-file-truename)
                                                    (car (split-string  buffer-file-truename (diego/current-project-name) t nil))
                                                  buffer-file-truename) "] ")
                                      (concat " Buffer: [" (buffer-name) "] ")))
                            'face 'modus-themes-subtle-blue) ignore))))

  (defun diego-tab-format-keycast ()
    "Format `mode-line-modified' for the tab bar."
    `((global menu-item ,(format-mode-line keycast-mode-line) ignore)))

  (defun diego-tab-format-line-break ()
    "Format a line break to simulate another row for the tab bar.
It needs an space before to stop any colour to follow at the end of the row."
    `((global menu-item " \n" ignore)))

  (setq tab-bar-format
        '(mode-line-front-space
          prot-tab-format-modified
          prot-tab-format-evil
          tab-bar-format-tabs ;; tab-bar-format-tabs-groups ; remove as it duplicates the tabs
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
