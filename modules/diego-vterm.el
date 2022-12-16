;;** vterm.el
(use-package vterm
  :demand t
  :bind (:map
         vterm-mode-map
         ("<f5>" . nil)
         ("<f6>" . nil)
         ("M-h"  . nil)
         ("M-j"  . nil)
         ("M-k"  . nil)
         ("M-l"  . nil)
         ("C-M-h"  . nil)
         ("C-M-j"  . nil)
         ("C-M-k"  . nil)
         ("C-M-l"  . nil)
         )
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 500000)

  (defun diego--vterm-hook ()
    ;; Don't prompt about dying processes when killing vterm
    (setq confirm-kill-processes nil)
    ;; Prevent premature horizontal scrolling
    (setq hscroll-margin 0))

  (cl-defun diego/vterm (&optional (buffername "*diego/vterm*") command)
    "Switch to (or create) a general vterm called *diego/vterm*.
Rename buffer if BUFFERNAME is passed, and run the passed COMMAND."
    (interactive)
    ;; HACK forces vterm to redraw, fixing strange artefacting in the tty.
    (let ((name (if current-prefix-arg (generate-new-buffer-name buffername) buffername)))
      (if (get-buffer name)
          (switch-to-buffer name)
        (with-current-buffer (get-buffer-create name)
          (switch-to-buffer name)
          (vterm-mode)
          (compilation-shell-minor-mode)))
      (when command
        (message "command: %s" command)
        (vterm-send-C-c)
        (vterm-send-string command)
        (vterm-send-return))))

  (defun diego/vterm-run-command (command &optional buffername)
    "Send a COMMAND to vterm.
Receives an optional BUFFERNAME to be used."
    (interactive
     (list (compilation-read-command nil) "*vterm*"))
    (diego/vterm buffername command))

  (defun diego/vterm-project ()
    "Switch to (or create) a vterm for current project and cd into project root."
    (interactive)
    (let* ((proj (project-current))
           (default-directory (if proj (project-root proj) "/tmp"))
           (name (diego/current-project-root))
           (buffername (format "*vterm-project: %s*" name)))
      (diego/vterm buffername)))

  (defun diego/vterm-run-current-line ()
    "Insert text of current line in vterm, execute and return focus to original buffer."
    (interactive)
    (let* ((line (if (use-region-p)
                     ;; current selection
                     (buffer-substring (region-beginning) (region-end))
                   ;; current line
                   (thing-at-point 'line t)))
           (command (string-trim line)))
      (save-window-excursion
        (diego/vterm-run-command command "*vterm*"))))

  :hook ((vterm-mode-hook . diego--vterm-hook)))

(provide 'diego-vterm)
