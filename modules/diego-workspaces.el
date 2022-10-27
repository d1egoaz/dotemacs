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
       (or (string-prefix-p "magit:" (buffer-name buffer)) ; special case for when the first project buffer is magit
           (buffer-local-value 'buffer-file-name (get-buffer buffer)))))

(defun diego/workspaces-name-for-buffer (buffer _a)
  "Select an appropriate tab name given a buffer BUFFER."
  (if-let ((file-name (buffer-file-name buffer))) ; check if buffer is visiting a file
      (if (file-remote-p file-name)
          "|tramp|" ; I don't use remote files too much, when I use them just use a single workspace
        (let* ((root-dir (diego--locate-project-root-dir file-name))
               (name (diego--root-dir-format-name root-dir)))
          name))
    (diego--root-dir-format-name (buffer-local-value 'default-directory (get-buffer buffer)))))

(defun diego--locate-project-root-dir (file-name)
  "Check if FILE-NAME is under a known project type.
Known project types are Git or a directory having a .project file."
  (or (locate-dominating-file file-name ".git")
      (locate-dominating-file file-name ".project")))

(defun diego--root-dir-format-name (root-dir)
  "Remove path prefixes for only known directories to keep the tab names short.
If ROOT-DIR matches a list of known paths returns only the
directory name, otherwise return the ROOT-DIR."
  (message "root-dir: %s" root-dir)
  (format "%s"
          (if (or (string-prefix-p "~/src/github.com/Shopify" root-dir)
                  (string-prefix-p (concat (getenv "HOME") "/src/github.com/Shopify") root-dir)
                  (string-prefix-p "/Volumes/GoogleDrive/My Drive" root-dir))
              (file-name-nondirectory (directory-file-name root-dir)) ; example: /tmp/foo/bar -> bar
            root-dir)))

(defun diego/reload-workspaces ()
  "Reload workspaces."
  (interactive)
  (tab-bar-mode -1)
  (tab-bar-mode 1))

(provide 'diego-workspaces)
;;; diego-workspaces.el ends here
