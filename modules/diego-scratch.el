;; ** Persistent Scratch buffers

(use-package emacs
  :straight (:type built-in)
  :config
  ;; *** Always start with a custom scratch buffer
  (setq initial-buffer-choice "~/scratch/*scratch*")

  ;; *** Open the scratch buffers when opening emacs.
  (defun diego--init-scratch-buffers ()
    (tab-bar-rename-tab "|scratch|") ;; create initial tab
    (kill-buffer "*scratch*") ;; in case that for some reason it opened the emacs scratch buffer and not my version
    (find-file "~/scratch/*scratch*" t)
    (with-current-buffer "*scratch*" ; Protect scratch buffer against accidental kill
      (emacs-lock-mode 'kill)))

  (add-hook 'after-init-hook #'diego--init-scratch-buffers)

  ;; *** Custom scratch buffers
  (setq diego--scratch-mode-list '(("org-mode" . ".org")
                                   ("emacs-lisp" . ".el")
                                   ("text-mode" . ".txt")
                                   ("markdown" . ".md")))

(defun diego--scratch-buffer-query-modes ()
  (alist-get
   (completing-read "Mode: " diego--scratch-mode-list)
   diego--scratch-mode-list nil nil 'equal))

    ;;;###autoload
     (defun diego/make-scratch (ext)
       "Get a scratch buffer for with the extension EXT and a random name.
    When called interactively with a prefix arg, prompt for the mode."
       (interactive (list (diego--scratch-buffer-query-modes)))
       (let* ((name (concat (make-temp-name (format "*scratch-")) ext)))
         (find-file (format "~/scratch/%s" name))
         (with-current-buffer (get-buffer-create name)
           (save-excursion
             (insert (format "Scratch buffer for: %s\n\n" ext))
             (goto-char (point-min))
             (comment-region (point-at-bol) (point-at-eol)))
           (forward-line 2))))

     (defun diego/make-new-scratch-buffer-go-babel ()
       "New temporary scratch buffer with a random name with go-babel enabled."
       (interactive)
       (diego/make-scratch ".org")
       (insert "
    \#+begin_src go
    package main
    import \"fmt\"
    func main() {
        fmt.Println(\"hello d1egoaz\")
    }
    \
    ")))

     (provide 'diego-scratch)
