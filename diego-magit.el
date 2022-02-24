;;; diego-magit.el --- Magit customization           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Diego Alvarez

;; Author: Diego Alvarez     (concat "git checkout -b " new_branch_name " origin/master")))) <diego.canada@icloud.com>
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

(require 'magit)
(require 'forge)

;;;###autoload
(defun diego/git-create-branch-from-origin-main ()
  "Create a new branch starting from origin/main."
  (interactive)
  (magit-fetch-branch "origin" "main" nil)
  (let ((new_branch_name (read-from-minibuffer "New branch name (from origin/main): " "d1egoaz_")))
    (magit-git-command-topdir
     (concat "git checkout -b " new_branch_name " origin/main"))))

;;;###autoload
(defun diego/git-create-branch-from-origin-master ()
  "Create a new branch starting from origin/master."
  (interactive)
  (magit-fetch-branch "origin" "master" nil)
  (let ((new_branch_name (read-from-minibuffer "New branch name (from origin/master): " "d1egoaz_")))
    (magit-git-command-topdir
     (concat "git checkout -b " new_branch_name " origin/master"))))

;;;###autoload
(defun diego/checkout-gh-pr (pr)
  "Checkouts a branch from a PR number."
  (interactive "nPR number: ")
  (magit-git-command-topdir
   (format "gh pr checkout %s" pr)))

;;;###autoload
(defun diego/fetch-and-rebase-onto-origin-master ()
  (interactive)
  (magit-fetch-branch "origin" "master" nil)
  (magit-git-rebase "origin/master" nil))

;;;###autoload
(defun diego/visit-pull-request-url ()
  "Visit the current branch's PR on Github.
Uses gh and magit"
  (interactive)
  (call-process
   "gh"
   nil
   0 ; <- Discard and don't wait
   nil
   "pr"
   "view"
   (magit-get-current-branch)
   "-w"))

(provide 'diego-magit)
;;; diego-magit.el ends here
