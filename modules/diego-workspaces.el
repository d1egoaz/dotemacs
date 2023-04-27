;;; diego-workspaces.el --- Diego Workspaces/tabs Related Functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Diego Alvarez

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

;; Usage example:

;; IMPORTANT: enable `tab-bar-mode' first.

;; (push '(diego/workspaces-validate-buffer
;;         .
;;         (display-buffer-in-tab (tab-name . diego/workspaces-name-for-buffer)))
;;       display-buffer-alist)

;; or add it directly to display-buffer-alist

;; (setq display-buffer-alist
;;       `(
;;         ;; more stuff
;;         (diego/workspaces-validate-buffer
;;          (display-buffer-in-tab)
;;          (tab-name . diego/workspaces-name-for-buffer))
;;         ;; more stuff
;;         ))
;;
;; ;; to close all project's buffers automatically when closing the tab.
;;   (setq tab-bar-tab-pre-close-functions '(diego--close-workspace))

;;; Code:

(defvar diego-workspaces-enabled t)

(defun diego/toggle-workspaces-enabled ()
  "Toggle workspaces by tabs."
  (interactive)
  (setq diego-workspaces-enabled (not diego-workspaces-enabled))
  (message "Workspaces tabs %s" (if diego-workspaces-enabled "enabled" "disabled")))

(defun diego/workspaces-validate-buffer (buffer &rest _)
  "Check if workspaces per buffer is enabled and that BUFFER is visiting a file."
  (and diego-workspaces-enabled
       (or (string-prefix-p "magit:" buffer) ; special case for when the first project buffer is magit
           (buffer-local-value 'buffer-file-name (get-buffer buffer)))))

(defun diego/workspaces-name-for-buffer (buffer _a)
  "Select an appropriate tab name given a buffer BUFFER."
  (if-let ((file-name (buffer-file-name buffer))) ; check if buffer is visiting a file
      (if (file-remote-p file-name)
          "|tramp|" ; I don't use remote files too much, when I use them just use a single workspace
        (let* ((root-dir (diego--locate-project-root-dir file-name))
               (name (diego--root-dir-format-name root-dir file-name)))
          name))
    (diego--root-dir-format-name (buffer-local-value 'default-directory (get-buffer buffer)) file-name)))

(defun diego--locate-project-root-dir (file-name)
  "Check if FILE-NAME is under a known project type.
Known project types are Git or a directory having a .project file."
  (or (locate-dominating-file file-name ".project")
      (locate-dominating-file file-name ".git")))

(defun diego--root-dir-format-name (root-dir file-name)
  "Remove path prefixes for only known directories to keep the tab names short.
If ROOT-DIR matches a list of known paths returns only the
directory name, otherwise return the ROOT-DIR."
  (message "root-dir: %s" root-dir)
  (unless root-dir
    (setq root-dir (file-name-directory file-name)))
  (setq root-dir (expand-file-name root-dir)) ;; to converts path that uses ~ or $HOME
  (file-name-nondirectory (directory-file-name root-dir)))

(defun diego/reload-workspaces ()
  "Reload workspaces (tab-bar)."
  (interactive)
  (tab-bar-mode -1)
  (tab-bar-mode 1))

(defun diego--close-workspace (tab _bool)
  "Close the TAB workspace.
This is intended to be called via `tab-bar-tab-pre-close-functions'."
  (let ((name (alist-get 'name tab)))
    (when (string= "*kubel*" name)
      (kill-matching-buffers "\\*kubel" nil t))
    (when (project-current)
      (project-kill-buffers t))
    (message "Project %s closed." name)))

(provide 'diego-workspaces)
;;; diego-workspaces.el ends here
