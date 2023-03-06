(use-package request)
(use-package json)

(defvar chatgpt--must-use-markdown "response must use markdown, and code blocks must use the right language tag")

(defun chatgpt-append-result (str)
  "Insert result STR of the chatgpt-query at the end of buffer *ChatGPT*."
  (let ((buf (get-buffer-create "*ChatGPT*")))
    (with-current-buffer buf
      (gfm-mode)
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
                                 (chatgpt-append-result (format "### 🤖Response %s\n" (let ((message-content (aref (cdr (assoc 'choices data)) 0)))
                                                                                        (cdr (assoc 'content (cdr (assoc 'message message-content))))))))))))
    (chatgpt-append-result (format "-----\n# %s\n## ❓%s" (format-time-string "%H:%M:%S PT") input))))

(defun chatgpt ()
  "Interact with the ChatGPT API and display the response."
  (interactive)
  (let ((input (read-string "Enter your input: ")))
    (chatgpt-query (format "%s, %s" input chatgpt--must-use-markdown))))

(defun chatgpt-summarize ()
  "Summarize the selected text or prompt for input and summarize."
  (interactive)
  (chatgpt--action-on-text "Summarize this text" "Enter text to summarize: "))

(defun chatgpt-explain-code ()
  "Summarize the selected text or prompt for input and summarize."
  (interactive)
  (chatgpt--action-on-text (format "Explain this code, %s" chatgpt--must-use-markdown) "Enter code to explain: "))

(defun chatgpt-rewrite ()
  "Summarize the selected text or prompt for input and summarize."
  (interactive)
  (chatgpt--action-on-text "Rewrite the following text" "Enter text to rewrite: "))

(defun chatgpt-gen-test ()
  "Summarize the selected text or prompt for input and summarize."
  (interactive)
  (chatgpt--action-on-text (format "Generate unit tests for the following code, %s" chatgpt--must-use-markdown) "Enter code to explain: "))

(defun chatgpt--action-on-text (action action-query)
  "Summarize the selected text or prompt for input and summarize."
  (let ((text (if (use-region-p)
                  (buffer-substring (region-beginning) (region-end))
                (read-string action-query))))
    (chatgpt-query (format "%s:\n%s" action text))))

(provide 'diego-chatgpt)
