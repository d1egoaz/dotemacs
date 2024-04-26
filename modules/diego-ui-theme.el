(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs nil)
  (setq modus-themes-bold-constructs nil)
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-prompts '(italic bold))
  (setq modus-themes-completions '((matches . (extrabold underline)) (selection . (semibold))))

  (defun diego--improve-colors ()
    (modus-themes-with-colors
     (custom-set-faces
      `(web-mode-block-delimiter-face ((,c :foreground ,yellow)))
      `(tab-bar
        ((,c :box (:line-width (12 . 8) :color nil :style flat-button) :font "SF Pro Text-22")))
      `(tab-bar-tab ((,c :background ,bg-blue-nuanced :foreground ,blue :overline ,blue :bold nil)))
      `(modern-tab-bar-tab
        ((,c :background ,bg-blue-nuanced :foreground ,blue :overline ,blue :bold nil)))
      `(tab-bar-tab-inactive ((,c :background ,bg-cyan-subtle :overline ,blue :bold nil)))))


    (set-face-attribute 'font-lock-number-face nil :foreground "#ff9580" :bold nil)
    (set-face-attribute 'font-lock-escape-face nil :foreground "#feacd0")
    (set-face-attribute 'font-lock-function-call-face nil :inherit nil :foreground "#00bcff" :bold nil)
    (set-face-attribute 'font-lock-function-name-face nil :background "#004065" :foreground "#00d3d0"))

  ;; similar to Dark Modern
  ;; (setq modus-vivendi-palette-overrides '((bg-main "#212121")
  ;;                                         (fg-main "#c2c2c2")
  ;;                                         (comment  "#6a9955")
  ;;                                         (constant "#569cd6")
  ;;                                         (fnname "#dcdcaa")
  ;;                                         (string "#ce9178")
  ;;                                         (variable "#9cdcfe")))

  ;; tmp ;; (keyword  "#C678DD") ;; (keyword  "#d19a66")

  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)
  ;; (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-faint)
  ;; (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-cooler)
  ;; (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-warmer)

  ;; dark theme, mixed of One Dark Pro and Dark plus
  (setq modus-vivendi-palette-overrides
        '((bg-main "#23272e")
          (fg-main "#abb2bf")
          (string "#98C379")
          (type "#6ae4b9")
          (keyword "#b6a0ff")
          (variable "#d4d4d4")
          (comment "#7f848e")
          (constant "#9cdcfe")))

  :bind ("<f5>" . #'modus-themes-toggle)
  :hook (modus-themes-after-load-theme-hook . diego--improve-colors))

(provide 'diego-ui-theme)
