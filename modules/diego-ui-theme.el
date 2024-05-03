(use-package ef-themes
  :straight (ef-themes :type git :host github :repo "protesilaos/ef-themes")
  :config
  (setq ef-themes-headings ; read the manual's entry or the doc string
        '((0 variable-pitch light 1.9)
          (1 variable-pitch light 1.8)
          (2 variable-pitch regular 1.7)
          (3 variable-pitch regular 1.6)
          (4 variable-pitch regular 1.5)
          (5 variable-pitch 1.4) ; absence of weight means `bold'
          (6 variable-pitch 1.3)
          (7 variable-pitch 1.2)
          (t variable-pitch 1.1)))

  (setq ef-themes-mixed-fonts t)
  (setq ef-themes-variable-pitch-ui t)
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-dream)

  ;; (setq ef-summer-palette-overrides
  ;;     '((cursor "#ef9050")
  ;;       (comment red-faint)
  ;;       (keyword cyan-cooler)))
  (setq ef-themes-common-palette-overrides '((bg-region bg-magenta-intense) (fg-region fg-intense)))

  ;; (ef-themes-with-colors
  ;; (list bg-main fg-main bg-mode-line cursor))

  ;; (ef-themes-get-color-value 'variable)
  ;; => "#5250ef"

  ;; Read from the overrides and deal with any recursion to find the
  ;; underlying value.
  ;; (ef-themes-get-color-value 'variable :overrides)

  (defun my-ef-themes-custom-faces ()
    "My customizations on top of the Ef themes.
This function is added to the `ef-themes-post-load-hook'."

      (ef-themes-with-colors
     (custom-set-faces
      `(web-mode-block-delimiter-face ((,c :foreground ,yellow)))
      `(tab-bar ((,c :weight light :box (:line-width 4 :style flat-button) :font "SF Pro Text-22")))
      `(tab-bar-tab ((,c :background ,bg-blue-intense :foreground ,blue :overline ,blue :bold nil)))
      `(tab-bar-tab-inactive ((,c :background ,bg-cyan-subtle :overline ,blue :bold nil :box (:line-width 2 :color nil :style pressed-button))))))


    (set-face-attribute 'font-lock-number-face nil :foreground "#ff9580" :bold nil)
    (set-face-attribute 'font-lock-escape-face nil :foreground "#feacd0")
    (set-face-attribute 'font-lock-function-call-face nil
                        :inherit nil
                        :foreground "#00bcff"
                        :bold nil)
    (set-face-attribute 'font-lock-function-name-face nil :background "#004065" :foreground "#00d3d0")
    (set-face-attribute 'font-lock-builtin-face nil :bold nil)
)


  ;; Using the hook lets our changes persist when we use the commands
  ;; `ef-themes-toggle', `ef-themes-select', and `ef-themes-load-random'.
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-custom-faces)


  (defun my-ef-themes-mode-line ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
     (custom-set-faces
      ;; `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
      `(mode-line ((,c :background ,bg-mode-line :foreground ,fg-mode-line :box (:line-width 1 :color ,fg-dim))))

      `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-mode-line))


;; ef-dream background, foreground, comment
;; ef-night nice

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs nil)
  (setq modus-themes-bold-constructs nil)
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-prompts '(italic bold))
  (setq modus-themes-completions '((matches . (extrabold underline)) (selection . (semibold))))

  (defun diego--improve-colors ()
    ;; format-next-line: off
    ;; (modus-themes-with-colors
  (ef-themes-with-colors
     (custom-set-faces
      `(web-mode-block-delimiter-face ((,c :foreground ,yellow)))
      `(tab-bar ((,c :weight light :box (:line-width 4 :style flat-button) :font "SF Pro Text-22")))
      `(tab-bar-tab ((,c :background ,bg-blue-intense :foreground ,blue :overline ,blue :bold nil)))
      `(tab-bar-tab-inactive ((,c :background ,bg-cyan-subtle :overline ,blue :bold nil :box (:line-width 2 :color nil :style pressed-button))))))


    (set-face-attribute 'font-lock-number-face nil :foreground "#ff9580" :bold nil)
    (set-face-attribute 'font-lock-escape-face nil :foreground "#feacd0")
    (set-face-attribute 'font-lock-function-call-face nil
                        :inherit nil
                        :foreground "#00bcff"
                        :bold nil)
    (set-face-attribute 'font-lock-function-name-face nil
                        :background "#004065"
                        :foreground "#00d3d0"))

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
