;;; diego-windows.el --- Common Windows Functions -*- lexical-binding: t; -*-

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

(defgroup diego-windows ()
  "Auxiliary functions for my dotemacs."
  :group 'editing)
;; Copied from doom emacs
;;;###autoload
(defun +evil/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

;;;###autoload
(defun +evil/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

(define-minor-mode diego/toggle-window-dedication
  "Minor mode for dedicating windows.
This minor mode dedicates the current window to the current buffer.
The code is taken from here: https://github.com/skeeto/.emacs.d/blob/master/lisp/extras.el"
  :init-value nil
  :lighter " [D]"
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;; from https://www.reddit.com/r/emacs/comments/pka1sm/my_first_package_aside_for_easier_configuration/hc3g1z7
(cl-defun diego/display-buffer-in-side-window (&optional (buffer (current-buffer)))
  "Display BUFFER in dedicated side window."
  (interactive)
  (let ((display-buffer-mark-dedicated t))
    (display-buffer-in-side-window buffer
                                   '((side . right)
                                     (window-parameters
                                      (no-delete-other-windows . t))))))


;; Swap windows if there are two of them
;; copied from https://github.com/karthink/.emacs.d/blob/master/lisp/better-buffers.el
;;;###autoload
(defun diego/swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (cl-first (window-list)))
                (w2 (cl-second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))


(defun diego/window-remove-side-parameter ()
  (interactive)
  (set-window-parameter nil 'window-side nil))

(provide 'diego-windows)
;;; diego-windows.el ends here
