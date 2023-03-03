;;; diego-common.el --- Common Functions -*- lexical-binding: t; -*-

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

(defun diego/indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun diego/delete-last-char-eol ()
  "Delete last character in line."
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (delete-char -1)))

(defvar diego--emojis '(
                        "ಠ_ಠ"
                        "¯\\_(ツ)_/¯"
                        "(╯°□°）╯︵ ┻━┻"
                        "(⌐■_■)"
                        "¯\\(°_°)/¯"))

(defun diego/emoji-insert ()
  (interactive)
  (insert (completing-read "Emoji to insert: " diego--emojis)))

(defun diego/copy-buffer-name ()
  "copy buffer name"
  (interactive)
  (let ((b (buffer-name)))
    (message b)
    (kill-new b)))

(defun diego/copy-file-name ()
  "copy buffer name"
  (interactive)
  (let ((path (buffer-file-name)))
    (message path)
    (kill-new path)))

(defun diego/copy-buffer-dir-path ()
  "copy buffer path to clipboard"
  (interactive)
  (message default-directory)
  (kill-new default-directory))

(defun diego/url-to-markdown-image ()
  "Copy URL from clipboard and creates and image tag to use in markdown"
  (interactive)
  (kill-new
   (format "<img src=\"%s\" width=\"50%%\" />" (current-kill 0))))

(defun diego/today-UTC-date ()
  "copy the full UTC time to clipboard"
  (interactive)
  "Inserts the current date in the buffer"
  ;; nil to use current date, t to use UTC
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))

(defun diego/now ()
  "Inserts the current time in the buffer"
  (interactive)
  (insert (format-time-string "%H:%M:%S PT")))

(defun diego--exec-command-replace-region (command)
  (unless mark-active
    (progn
      ;; (mark-whole-buffer) replacement
      (push-mark)
      (push-mark (point-max) nil t)
      (goto-char (point-min))))
  (message "running %s" command)
  (shell-command-on-region
   (region-beginning) (region-end)
   command
   (current-buffer) t "*diego/error-buffer*" t))

(defun diego/prettify-json ()
  "prettify json current region"
  (interactive)
  (diego--exec-command-replace-region "jq -SM ."))

(defun diego/resize-image ()
  (interactive)
  (let ((str (concat "convert \"" buffer-file-name "\" -geometry x300 \"" buffer-file-name "\"")))
    (message str)
    (shell-command-to-string str)))

(defun diego/prettify-jsonv-with-prettier ()
  "prettify json current region"
  (interactive)
  (diego--exec-command-replace-region "prettier --parser json"))

(defun diego/prettify-markdown ()
  "prettify markdown current region"
  (interactive)
  (diego--exec-command-replace-region "prettier --parser markdown"))

(defun diego/prettify-yaml ()
  "prettify yaml current region"
  (interactive)
  (diego--exec-command-replace-region "prettier --parser yaml"))

(defun diego/kill-close-all-buffers ()
  (interactive)
  (let ((keep '("*scratch* *Messages*")))
    (switch-to-buffer "*scratch*")
    (delete-other-windows)
    (mapc
     (lambda (b)
       (unless (member (buffer-name b) keep)
         (kill-buffer b)))
     (buffer-list))))

(defun diego/kill-buffer ()
  "Kill a buffer and return to previous recent buffer."
  (interactive)
  (let ((b (current-buffer)))
    (if (not (project-current))
        (tab-bar-close-tab)
      ;; Switch to the most recently selected buffer other than the current one.
      (mode-line-other-buffer)) ;; move to recent buffer first
    (kill-buffer b)))

(defun diego/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun diego/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "buffer '%s' copied to kill ring" (buffer-name)))

(defun diego/evil-insert-line-above (count)
  "Insert one or several lines above the current point's line without changing
    the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun diego/evil-insert-line-below (count)
  "Insert one or several lines below the current point's line without changing
    the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun diego/autocapitalize-org-headings ()
  "Find org headings and capitalize first word"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (org-next-visible-heading 1)
      (forward-to-word 1)
      (while (member (word-at-point) '("TODO" "DONE" "CANCELLED"))
        (forward-to-word 1))
      (upcase-char 1))))

(defun diego/insert-uuid ()
  (interactive)
  (insert (shell-command-to-string "uuidgen")))

(defun diego/insert-filename ()
  "Insert a filename at point."
  (interactive)
  (insert (read-file-name "File:")))

(defun diego/insert-relative-filename ()
  "Insert a relative filename at point."
  (interactive)
  (insert (file-relative-name (read-file-name "File: "))))

(defun diego/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (browse-url (concat "file://" file-name))))

(defun diego/what-the-commit ()
  (interactive)
  (insert
   (with-current-buffer
       (url-retrieve-synchronously "http://whatthecommit.com")
     (re-search-backward "<p>\\([^<]+\\)\n<\/p>")
     (match-string 1))))

(defun diego/make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "alfredoc") (width . 80) (height . 16)
                (top . 400) (left . 300)))
  (select-frame-by-name "alfredoc")
  (org-capture))

(defun diego/consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun diego/go-run-test-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (move-end-of-line nil)
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (compile (concat "go test -v -run=" (match-string-no-properties 3)) t))
    (message "Must be in a _test.go file to run go-run-test-current-function")))

;; from https://github.com/alphapapa/unpackaged.el
(defun diego/org-fix-blank-lines (&optional prefix)
  "Ensure that blank lines exist between headings and between headings and their contents.
With prefix, operate on whole buffer. Ensures that blank lines
exist after each headings's drawers."
  (interactive "P")
  (org-map-entries (lambda ()
                     (org-with-wide-buffer
                      ;; `org-map-entries' narrows the buffer, which prevents us from seeing
                      ;; newlines before the current heading, so we do this part widened.
                      (while (not (looking-back "\n\n" nil))
                        ;; Insert blank lines before heading.
                        (insert "\n")))
                     (let ((end (org-entry-end-position)))
                       ;; Insert blank lines before entry content
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers. You might think that `org-at-drawer-p' would suffice, but
                         ;; for some reason it doesn't work correctly when operating on hidden text.
                         ;; This works, taken from `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n"))))
                   t (if prefix
                         nil
                       'tree)))

(defun diego--lookup-provider-alist ()
  (append
   diego--shopify-lookup-provider-alist
   '(("Github Search" "https://github.com/search?q=%s")
     ("Github NEW Search" "https://cs.github.com/?scopeName=All+repos&scope=&q=%s")
     ("Github open Shopify Repository" "https://github.com/Shopify/%s")
     ("Github open Shopify Repository .dev (code)" "https://github.dev/Shopify/%s")
     ("Github Shopify Search" "https://github.com/search?q=org:Shopify+%s")
     ("Github Shopify NEW Search" "https://cs.github.com/?scope=org:Shopify&scopeName=Shopify&q=%s")
     ("Google" "https://google.com/search?q=%s"))))

(defun diego/open-url ()
  "Open an URL given a provider and a query."
  (interactive)
  (let ((site (cadr (assoc
                     (completing-read "Search on: " (diego--lookup-provider-alist))
                     (diego--lookup-provider-alist)))))
    (browse-url
     (url-encode-url
      (format site (read-from-minibuffer "Query: "))))))

(defun diego/delete-file ()
  "Removes file and buffer."
  (interactive)
  (if-let ((filename (buffer-file-name))
           (buffer (current-buffer))
           (name (buffer-name)))
      (if (file-exists-p filename) ; it can be a new file that is not saved yet
          (when (yes-or-no-p (format "Are you sure you want to delete '%s' ? " filename))
            (delete-file filename)
            (kill-buffer buffer)
            (message "File '%s' successfully deleted" filename))
        (message "File '%s' doesn't exist" filename))
    (message "Buffer '%s' is not visiting a file" (buffer-name))))

(defun diego/auth-source-get-password (host user)
  "Get password for HOST and USER from .authinfo.gpg file."
  (let* ((secret
          (plist-get
           (car (auth-source-search :max 1 :host host :user user))
           :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(provide 'diego-common)
;;; diego-common.el ends here
