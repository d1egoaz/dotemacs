(use-package emacs
  :straight (:type built-in)
  :config
  ;; Fontconfig pattern, fontname[-fontsize][:name1=values1][:name2=values2]...
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
  (add-to-list 'default-frame-alist '(font . "Iosevka SS08-14:weight=medium"))

  (setq-default line-spacing 1) ; needs to be changed for some fonts

  (set-face-attribute 'fixed-pitch nil
                      :font "-*-Iosevka SS08-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'variable-pitch nil
                      :font "SF Pro Text-14")
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
                      :slant 'italic)

  ;; *** Configure emoji font 😎
  (set-fontset-font t 'emoji '("Apple Color Emoji" . "iso10646-1") nil 'prepend)

  ;; *** Zooming In and Out
  (global-set-key (kbd "s-=") #'text-scale-increase)
  (global-set-key (kbd "s--") #'text-scale-decrease)

  ;; Helps when scrolling images, as Emacs treats pictures as a single characters.
  (pixel-scroll-precision-mode 1))

;;** mixed-pitch.el
(use-package mixed-pitch
  :hook (
         (org-mode-hook       . mixed-pitch-mode)
         (gfm-mode-hook        . mixed-pitch-mode)))

(provide 'diego-ui-font)