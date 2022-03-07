;;; diego-vterm.el --- Diego Project Related Functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Diego Alvarez

;; Author: Diego Alvarez <diego.canada@icloud.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'vterm)

;;;###autoload
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

;;;###autoload
(defun diego/vterm-project ()
  "Switch to (or create) a vterm for current project and cd into project root."
  (interactive)
  (let* ((proj (project-current))
         (default-directory (if proj (project-root proj) "/tmp"))
         (name (diego/current-project-name))
         (buffername (format "*vterm-project: %s*" name)))
    (diego/vterm buffername)))

;;;###autoload
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

(provide 'diego-vterm)
