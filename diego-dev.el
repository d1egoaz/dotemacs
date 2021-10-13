;;; diego-dev.el --- dev                             -*- lexical-binding: t; -*-

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

;;;###autoload
(defun diego/dev ()
  "Runs a common dev command. Or run the passed command if no command was selected"
  (declare (interactive-only compile))
  (interactive)
  (let ((list (mapcar (lambda (str)
                        (concat "/opt/dev/bin/dev " str))
                      '("up" "down" "build" "test" "style" "runtime refresh"))))
    (compile
     (completing-read "Dev command: " list  nil nil nil 'compile-history) t)))

;;;###autoload
(defun diego/dev-project ()
  "Runs a dev command from the project defined commands."
  (declare (interactive-only compile))
  (interactive)
  (let* ((devopts (split-string
                   (shell-command-to-string "/opt/dev/bin/dev help --project | grep dev | tail -n +2 | cut -d' ' -f4 | sort")
                   "\n"))
         (list (mapcar (lambda (str)
                         (concat "/opt/dev/bin/dev " str))
                       devopts)))
    (compile
     (completing-read "Project Dev Command: " list nil nil nil 'compile-history) t)))

(provide 'diego-dev)
;;; diego-dev.el ends here
