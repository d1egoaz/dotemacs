;;; diego-project.el --- Diego Project Related Functions  -*- lexical-binding: t; -*-

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

(require 'project)

(defvar diego--project-project-roots '("~/src/github.com/Shopify" "~/code/oss"))

(defun prot-project--list-projects ()
  "Produce list of projects in `prot-project-project-roots'."
  (let* ((dirs diego--project-project-roots )
         (dotless directory-files-no-dot-files-regexp)
         (cands (mapcan (lambda (d)
                          (directory-files d t dotless))
                        dirs)))
    (mapcar (lambda (d)
              (list (abbreviate-file-name d)))
            cands)))

;; run to update project list
;; run project-remember-projects-under on ~
;; then run prot-project-add-projects
(defun prot-project-add-projects ()
  "Append `prot-project--list-projects' to `project--list'."
  (interactive)
  (project--ensure-read-project-list)
  (let ((projects (prot-project--list-projects)))
    (setq project--list (append projects project--list))
    (project--write-project-list)))

;;;###autoload
(defun diego/open-project-readme ()
  "Open the README.md file in a project."
  (interactive)
  (find-file (expand-file-name "README.md" (diego/current-project-name))))

;;;###autoload
(defun diego/project-compile-dwim (command)
  "Run `compile' in the project root."
  (declare (interactive-only compile))
  (interactive)
  (let ((default-directory (diego/current-project-name))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (compile command t)))

;;;###autoload
(defun diego/project-compile ()
  "Run `compile' in the project root."
  (declare (interactive-only compile))
  (interactive)
  (let ((default-directory (diego/current-project-name))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (compile (completing-read "Compile command: " compile-history nil nil nil 'compile-history) t)))

;;;###autoload
(defun diego/recompile ()
  "Function has been almost copied from the original recompile.
It has been modified to always run on comint mode."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((default-directory (or compilation-directory default-directory))
        (command (eval compile-command)))
    (apply #'compilation-start (list command t nil nil)))) ; make sure to always use comint mode

;;;###autoload
(defun diego/current-project-name ()
  (when-let ((proj (project-current)))
    (project-root proj)))

;;;###autoload
(defun diego/project-short-name (root-dir)
  (if (or (string-prefix-p "~/src/github.com/Shopify" root-dir)
          (string-prefix-p "/Volumes/GoogleDrive/My Drive" root-dir))
      (file-name-nondirectory (directory-file-name root-dir)) ;; remove last / and get only dir name
    root-dir))

;;;###autoload
(defun diego/tab-name-for-buffer (b _a)
  (let ((bufname (buffer-file-name b)))
    (if (and buffer-file-truename bufname (file-remote-p bufname))
        "📔tramp"
      (if-let* ((project (project-current nil bufname)) ; project for file
                (root-dir (project-root project)) ; get only root dir
                (name (diego/project-short-name root-dir))) ; get only dir name
          (format "📔%s" name)
        "📔general"))))

(provide 'diego-project)
;;; diego-project.el ends here
