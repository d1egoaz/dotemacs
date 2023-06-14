(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs nil)
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-prompts '(italic bold))
  (setq modus-themes-org-blocks 'tinted-background)
  (setq modus-themes-completions
        '((matches . (extrabold underline))
          (selection . (semibold))))

  (defun diego--improve-colors ()
    (modus-themes-with-colors
      (custom-set-faces
       `(web-mode-block-delimiter-face ((,c :foreground ,yellow)))
       `(tab-bar-tab ((,c :background ,bg-blue-nuanced :foreground ,blue :overline ,blue :bold nil)))
       `(tab-bar-tab-inactive ((,c :background ,bg-cyan-subtle :overline ,blue :bold nil)))))

    (set-face-attribute 'font-lock-number-face nil :foreground "#d19a66" :bold nil)
    (set-face-attribute 'font-lock-escape-face nil :foreground "#dcdcaa")
    (set-face-attribute 'font-lock-function-call-face nil :inherit nil :foreground "#56b6c2")
    (set-face-attribute 'font-lock-function-name-face nil :background "#004065" :foreground "#00d3d0"))

  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)

  ;; similar to Dark Modern
  ;; (setq modus-vivendi-palette-overrides '((bg-main "#212121")
  ;;                                         (fg-main "#c2c2c2")
  ;;                                         (comment  "#6a9955")
  ;;                                         (constant "#569cd6")
  ;;                                         (fnname "#dcdcaa")
  ;;                                         (string "#ce9178")
  ;;                                         (variable "#9cdcfe")))

  ;; Similar to One Dark Pro Darker
  (setq modus-vivendi-palette-overrides '((bg-main "#23272e")
                                          (fg-main "#abb2bf")
                                          (fnname "#79a8ff")
                                          (variable "#9cdcfe")
                                          (type "#feacd0")

                                          (string "#98C379")
                                          (comment  "#7f848e")
                                          ;; (constant "#D19A66")
                                          (keyword  "#C678DD")
                                          ))

  (load-theme 'modus-vivendi :no-confirm)
  ;; (modus-themes-select 'modus-vivendi)

  :bind ("<f5>" . #'modus-themes-toggle)
  :hook (modus-themes-after-load-theme-hook . diego--improve-colors))

(provide 'diego-ui-theme)
