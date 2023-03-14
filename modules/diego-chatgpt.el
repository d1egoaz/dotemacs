;;; diego-chartgpt.el --- Common Functions -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Diego Alvarez

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

;; ----------------------------------------
;; EXAMPLE RESULT:
;; ./diego-chatgpt.png
;; ----------------------------------------

;;; Code:
(require 'json)
(require 'markdown-mode)

(defvar chatgpt--developer-role "You are a large language model living inside Emacs, and the perfect programmer.
Use a role of a Software Developer and Software Architect.
Response MUST be concise.
Response MUST use full and well written markdown, code blocks must use the right language tag.")

(defvar chatgpt--writter-role "You are a large language model living inside Emacs, and the perfect writing assistance,
your background is a Software Developer and Software Architect.
Response MUST be concise.")

(defun chatgpt-append-result (str)
  "Insert result STR of the chatgpt-query at the end of buffer *ChatGPT*."
  (save-window-excursion
    (let ((buf (find-file "~/scratch/*ChatGPT*")))
      (with-current-buffer buf
        (gfm-mode)
        (goto-char (point-max))
        (insert (concat "\n" str))
        (save-buffer)))))

(defun chatgpt--extract-content-response (_status callback &rest args)
  "Extract the last lines of a JSON string from a buffer.
Call user's CALLBACK with the result and passes the aditional ARGS."
  (goto-char 0)
  (re-search-forward "^$") ; Skip http result headers
  (let* ((json-string (buffer-substring (point) (point-max)))
         (json-object (json-read-from-string json-string))
         (message-content (aref (cdr (assoc 'choices json-object)) 0))
         (content (cdr (assoc 'content (cdr (assoc 'message message-content))))))
    (message "🤖: %s" content) ;; debug
    (apply callback content args)))

(defun chatgpt-request-open-api (input role callback &rest args)
  "Send an INPUT request to OpenAI API with ROLE, get result via CALLBACK.
Pass additional ARGS to the CALLBACK function."
  (interactive
   (list (read-string "Enter your input: ")
         'writter
         (lambda (response) (message "🤖: %s" response))))
  (let* ((api-key (diego/auth-source-get-password "api.openai.com" "apikey"))
         (sys-content (if (eq role 'dev) chatgpt--developer-role chatgpt--writter-role))
         (url "https://api.openai.com/v1/chat/completions")
         (model "gpt-3.5-turbo")
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,(format "Bearer %s" api-key))))
         ;; needed to use us-ascii, instead of utf-8 due to a multibyte text issue
         (url-request-data (encode-coding-string (json-encode
                                                  `(:model ,model
                                                           :messages [(:role "system" :content ,sys-content)
                                                                      (:role "user" :content ,input)]
                                                           :temperature 0.7)) 'us-ascii)))
    (url-retrieve url
                  #'chatgpt--extract-content-response
                  (list callback args))))

(defun chatgpt-rewrite-replace (beg end)
  "Rewrite the region BEG END and replace the selection with the result."
  (interactive "r")
  (let ((input (concat "Please rewrite the following text:\n" (buffer-substring-no-properties beg end))))
    (chatgpt-request-open-api input
                              'writter
                              (lambda (result &rest args)
                                (message "args: %S" args)
                                (let* ((arguments (car args))
                                       (buf (nth 0 arguments)) ; gets buffer name
                                       (beg (nth 1 arguments)) ; this is the bef passed as additional arg
                                       (end (nth 2 arguments))
                                       )
                                  (with-current-buffer buf
                                    (save-excursion
                                      (delete-region beg end)
                                      (goto-char beg)
                                      (insert result "\n")))))
                              (buffer-name)
                              beg
                              end)))

(defun chatgpt-query (input mode)
  "Interact with the ChatGPT API with the INPUT using the role MODE.
Uses by default the writter mode."
  (interactive
   (list (read-string "Enter your input: ")
         'writter))
  (chatgpt-append-result (format "\n# %s\n## ❓ Query\n%s\n" (format-time-string "%A, %e %B %Y %T %Z") input))
  (chatgpt-request-open-api input
                            mode
                            (lambda (result &rest args)
                              (chatgpt-append-result (format "### 🤖Response\n%s\n" result))
                              (pop-to-buffer "*ChatGPT*"))))

(defun chatgpt-dev-query ()
  "Interact with the ChatGPT API and display the response.  Using dev role."
  (interactive)
  (let ((input (read-string "Enter your input (dev role): ")))
    (chatgpt-query input 'dev)))

(defun chatgpt-summarize ()
  "Summarize the selected text or prompt for input and summarize."
  (interactive)
  (chatgpt--action-on-text "Summarize the following text" "Enter text to summarize: " 'writter))

(defun chatgpt-rewrite ()
  "Rewrite the selected text or prompt for input and rewrite."
  (interactive)
  (chatgpt--action-on-text "Rewrite the following text" "Enter text to rewrite: " 'writter))

(defun chatgpt-gen-test ()
  "Generate test for the passed text."
  (interactive)
  (chatgpt--action-on-text "Generate unit tests for the following code"  "Enter code to generate tests: " 'dev))

(defun chatgpt--action-on-text (action action-query role)
  "Execute ACTION on the selected text or prompt for input using ACTION-QUERY and execute the action.
Use the ROLE to tune the AI."
  (let ((text (if (use-region-p)
                  (buffer-substring (region-beginning) (region-end))
                (read-string action-query))))
    (chatgpt-query (format "%s:\n%s" action text) role)))

(defun chatgpt--get-buffer-role-as-tag ()
  "Get the current buffer mode as a string to be used as a tag for a markdown code block."
  (let ((str (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)) ))
    (if (string-suffix-p "-ts" str) ;; for the new *-ts-modes
        (substring str 0 (- (length str) 3))
      str)))

(defun chatgpt-explain-code ()
  "Explain the code for the selected text or prompt for input and explain."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring (region-beginning) (region-end))
                (read-string "Enter code to explain: "))))
    (chatgpt-query (format "Explain the following code, be concise:\n```%s\n%s```" (chatgpt--get-buffer-role-as-tag) text) 'dev)))

(provide 'diego-chatgpt)
;;; diego-chatgpt.el ends here
