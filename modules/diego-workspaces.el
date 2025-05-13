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

(defvar diego-workspaces-enabled nil)

(defun diego/toggle-workspaces-enabled ()
  "Toggle workspaces by tabs."
  (interactive)
  (setq diego-workspaces-enabled (not diego-workspaces-enabled))
  (message "Workspaces tabs %s"
           (if diego-workspaces-enabled
               "enabled"
             "disabled")))

(defun diego/workspaces-validate-buffer (buffer &rest _)
  "Check if workspaces per buffer is enabled and that BUFFER is visiting a file."
  (and diego-workspaces-enabled
       (or
        (string-prefix-p "magit:" buffer) ; special case for when the first project buffer is magit
        (buffer-local-value 'buffer-file-name (get-buffer buffer)))))

(defun diego/workspaces-name-for-buffer_old (buffer _a)
  "Select an appropriate tab name given a buffer BUFFER."
  (if-let* ((file-name (buffer-file-name buffer))) ; check if buffer is visiting a file
    (if (file-remote-p file-name)
        "|tramp|" ; I don't use remote files too much, when I use them just use a single workspace
      (let* ((root-dir (diego--locate-project-root-dir file-name))
             (name (diego--root-dir-format-name root-dir file-name)))
        name))
    (diego--root-dir-format-name
     (buffer-local-value 'default-directory (get-buffer buffer)) file-name)))

(defun diego--root-dir-format-name (root-dir file-name)
  "Remove path prefixes for only known directories to keep the tab names short.
If ROOT-DIR matches a list of known paths returns only the
directory name, otherwise return the ROOT-DIR."
  ;; (message "root-dir: %s" root-dir)
  (unless root-dir
    (setq root-dir (file-name-directory file-name)))
  (setq root-dir (expand-file-name root-dir)) ;; to converts path that uses ~ or $HOME
  (file-name-nondirectory (directory-file-name root-dir)))

(defun diego/workspaces-name-for-buffer (buffer &optional _ignore)
  "Return a concise 'workspace name' for BUFFER.
1) If BUFFER visits a remote file, return \"|tramp|\"
2) If BUFFER visits a local file in a project (.project or .git), use that project name
3) Otherwise, use the final directory name of the local file
4) If BUFFER doesn't visit a file, fall back to `default-directory'
5) If none of the above, return \"No-Directory\"."
  (let* ((filename    (buffer-file-name buffer))
         (default-dir (buffer-local-value 'default-directory buffer)))
    (cond
     ;; 1) Remote file
     ((and filename (file-remote-p filename))
      "|tramp|")

     ;; 2) Local file with project root
     ((and filename (diego--locate-project-root-dir filename))
      (diego--short-dir-name (diego--locate-project-root-dir filename)))

     ;; 3) Local file, but no project => just take the directory name
     (filename
      (diego--short-dir-name (file-name-directory filename)))

     ;; 4) If no file is visited, see if there's a default-directory
     (default-dir
      (diego--short-dir-name default-dir))

     ;; 5) Otherwise, fallback
     (t
      "No-Directory"))))

(defun diego--locate-project-root-dir (path)
  "Return the topmost directory containing .project or .git, starting at PATH.
Return nil if no such directory is found."
  (or (locate-dominating-file path ".project")
      (locate-dominating-file path ".git")))

(defun diego--short-dir-name (dir)
  "Expand DIR and return just its final directory name (no trailing slash)."
  (let ((expanded (expand-file-name dir)))  ;; handles ~, $HOME, etc.
    (file-name-nondirectory (directory-file-name expanded))))

(defun diego/reload-workspaces ()
  "Reload workspaces (tab-bar)."
  (interactive)
  (tab-bar-mode -1)
  (tab-bar-mode 1))


(defun diego--close-workspace (tab _)
  (let ((name (alist-get 'name tab)))
    (when (string= name "kubel")
      (remove-hook 'tab-bar-tab-pre-close-functions #'diego--close-workspace)
      (kill-matching-buffers "\\*kubel" nil t)
      (when (project-current)
        (project-kill-buffers t))
      (message "Project %s closed." name)
      (add-hook 'tab-bar-tab-pre-close-functions #'diego--close-workspace))))

(defun diego--close-workspace2 (tab _bool)
  "Close the TAB workspace.
This is intended to be called via `tab-bar-tab-pre-close-functions'."
  (let ((name (alist-get 'name tab)))
    (when (string= "kubel" name)
      (kill-matching-buffers "\\*kubel" nil t))
    (when (project-current)
      (project-kill-buffers t))
    (message "Project %s closed." name)))

(provide 'diego-workspaces)
;;; diego-workspaces.el ends here
