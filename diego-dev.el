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
  (let* ((prefix "/opt/dev/bin/dev ")
         (list (mapcar (lambda (str)
                        (concat prefix str))
                      '("up" "down" "build" "test" "style" "runtime refresh")))
        (cmd (completing-read "dev command: " list  nil nil prefix 'compile-history)))
    (compile cmd t)))

;;;###autoload
(defun diego/dev-project ()
  "Runs a dev command from the project defined commands."
  (declare (interactive-only compile))
  (interactive)
  (let* ((prefix "/opt/dev/bin/dev ")
         (devopts (split-string
                   (shell-command-to-string (concat prefix "help --project | grep dev | tail -n +2 | cut -d' ' -f4 | sort"))
                   "\n"))
         (list (mapcar (lambda (str)
                         (concat prefix str))
                       devopts)))
    (compile
     (completing-read "Project Dev Command: " list nil nil prefix 'compile-history) t)))

(provide 'diego-dev)
;;; diego-dev.el ends here
