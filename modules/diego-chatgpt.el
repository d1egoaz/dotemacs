(use-package request)
(use-package json)

(defun chatgpt-query (input)
  "Interact with the ChatGPT API and return the response."
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
                                 (message "🤖%s" (let ((message-content (aref (cdr (assoc 'choices data)) 0)))
                                                   (cdr (assoc 'content (cdr (assoc 'message message-content)))))))))))
    (message "❓%s\n" input)))

(defun chatgpt ()
  "Interact with the ChatGPT API and display the response."
  (interactive)
  (let ((input (read-string "Enter your input: ")))
    (chatgpt-query input)))

(provide 'diego-chatgpt)
