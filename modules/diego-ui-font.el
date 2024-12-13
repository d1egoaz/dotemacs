(use-package emacs
  :straight (:type built-in)
  :config
  ;; Fontconfig pattern, fontname[-fontsize][:name1=values1][:name2=values2]...
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
  ;; (add-to-list 'default-frame-alist '(font . "Essential PragmataPro-16:weight=regular"))
  (add-to-list 'default-frame-alist '(font . "Essential PragmataPro-16"))
  (set-fontset-font t nil "Essential PragmataPro-16" )

  ;; (set-face-attribute 'default nil :font "Essential PragmataPro-16" :height 100)

  ;; (setq-default line-spacing 1) ; needs to be changed for some fonts

  (set-face-attribute 'fixed-pitch nil :font "-*-Essential PragmataPro-regular-normal-normal-*-16-*-*-*-m-0-iso10646-1" :height 1.0)
  ;; (set-face-attribute 'variable-pitch nil :font "SF Pro Text-15")
  (set-face-attribute 'variable-pitch nil :font "Atkinson Hyperlegible-15" :height 1.0)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

  ;; *** Configure emoji font 😎
  (add-to-list 'face-font-rescale-alist '("Apple Color Emoji" . 0.8))
  (set-fontset-font t 'emoji '("Apple Color Emoji" . "iso10646-1") nil 'prepend)

  ;; *** Zooming In and Out
  (keymap-global-set "s-=" #'text-scale-increase)
  (keymap-global-set "s--" #'text-scale-decrease)

  ;; Helps when scrolling images, as Emacs treats pictures as a single characters.
  (setq pixel-scroll-precision-mode t))

(use-package mixed-pitch
  :hook ((org-mode-hook . mixed-pitch-mode) (gfm-mode-hook . mixed-pitch-mode)))

(provide 'diego-ui-font)
