;; -*- lexical-binding: t; -*-
(use-package emacs
  :straight (:type built-in)
  :config
  (setq ring-bell-function 'ignore)
  (setq visible-bell nil)

  ;; ** Increase fill column width
  (setq-default fill-column 100))

(provide 'diego-ui)
