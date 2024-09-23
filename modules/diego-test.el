(use-package json-mode)

(use-package jq-ts-mode
  :straight (:type git :host github :repo "nverno/jq-ts-mode"))

(use-package jq-shell
  :straight (:type git :host github :repo "nverno/jq-shell"))

(defun diego/copy-buffer-file-to-snippets-dir ()
  "Copy the current buffer's file to the interesting-scripts directory, keeping the original file name."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (target-dir "~/Documents/deft/interesting-scripts/")
         (file-name (file-name-nondirectory current-file))
         (target-file (concat target-dir file-name)))
    (if (not current-file)
        (message "Buffer is not visiting a file!")
      (copy-file current-file target-file t)
      (message "File copied to %s" target-file))))

(provide 'diego-test)
