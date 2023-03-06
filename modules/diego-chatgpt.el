(use-package request)
(use-package json)

(defun chatgpt-append-result (str)
  "Insert result STR of the chatgpt-query at the end of buffer *ChatGPT*."
  (let ((buf (get-buffer-create "*ChatGPT*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (insert (concat "\n" str))
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

(defun chatgpt-query (input)
  "Interact with the ChatGPT API with the INPUT and show the response."
  (let* ((api-key (diego/auth-source-get-password "api.openai.com" "apikey"))
         (url "https://api.openai.com/v1/chat/completions")
         (model "gpt-3.5-turbo")
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " api-key))))
         (response (request
                     url
                     :type "POST"
                     :headers headers
                     :data (json-encode
                            `(:model ,model :messages [(:role "user" :content ,input)]))
                     :parser 'json-read
                     :success (cl-function
                               (lambda (&key data &allow-other-keys)
                                 (chatgpt-append-result (format "🤖%s" (let ((message-content (aref (cdr (assoc 'choices data)) 0)))
                                                                         (cdr (assoc 'content (cdr (assoc 'message message-content))))))))))))
    (chatgpt-append-result (format "-----\n❓%s: %s" (format-time-string "%H:%M:%S PT") input))))

(defun chatgpt ()
  "Interact with the ChatGPT API and display the response."
  (interactive)
  (let ((input (read-string "Enter your input: ")))
    (chatgpt-query input)))

(provide 'diego-chatgpt)
