;; -*- lexical-binding: t; -*-
;;* AI - LLM

;; (use-package gptel
;;   :bind ((:map gptel-mode-map ("C-c C-c" . gptel-send)))
;;   :config
;;   (setq gptel-model "gpt-4o")
;;   (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
;;   (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

;; (use-package magit-gptcommit
;;   :after
;;   gptel
;;   magit) ;; magit-gptcommit-generate, magit-gptcommit-commit-create

(use-package c3po
  :straight (:host github :repo "d1egoaz/c3po.el" :branch "d1egoaz_test-llms")
  ;; :straight (:host github :repo "d1egoaz/c3po.el" :branch "main")
  :bind ("<f7>" . #'c3po-pop-results-buffer)
  :config

  ;; (setq c3po-model "gpt-3.5-turbo")
  ;; (setq c3po-model "claude-3-sonnet-20240229")
  (setq c3po-results-file-path "~/*🤖C3PO🤖*")
  ;; (setq c3po-temperature 0.1) ; 0.1 will make it more focused and deterministic.
  (setq c3po-temperature 0.6) ; 0.1 will make it more focused and deterministic.

  ;; (setq c3po-api-key (diego/auth-source-get-password "api.openai.com" "personal"))
  (setq c3po-api-key (diego/auth-source-get-password "api.openai.com" "chime"))
  ;; (setq c3po-api-key (diego/auth-source-get-password "api.anthropic.com" "personal"))

  (setq c3po-base-path-local-models "http://localhost:11434/v1/chat/completions")
  ;; format-next-line: off
  (c3po-add-local-models
   "qwen2.5:14b"
   "qwen2.5-coder:14b"
   "mistral-small"
   )
  (setq c3po-model "gpt-4o")
  ;; (setq c3po-model "mistral-small")
  ;; (setq c3po-model "qwen2.5-coder:14b")

  (c3po-add-new-droid '(synonymizer . (:system-prompt "
I want you to act as a synonyms provider.
I will give you a word, and you will reply with a list of synonym alternatives according to my prompt.
Provide a list of five synonyms per prompt, three short examples, and a list of five antonyms.
Please reply with only the word list, and nothing else, using this template:
**Synonyms:**
-

**Examples:**
-

**Antonyms:**
-
"))))

(provide 'diego-ai)
