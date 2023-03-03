(use-package emacs
  :straight (:type built-in)
  :config
  (setq ring-bell-function 'ignore)
  (setq visible-bell nil)

  ;; * UI - Let's make Emacs look a little better.

  ;; ** Disable toolbars and scrollbars

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode 1) ; I do like to have the menu-bar available to use when I break Emacs :D

  ;; ** Increase fill column width
  (setq-default fill-column 100)

  ;; (add-to-list 'default-frame-alist '(undecorated . t))
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

(provide 'diego-ui)
