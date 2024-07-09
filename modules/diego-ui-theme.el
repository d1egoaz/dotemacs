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
      ;; `(tab-bar ((,c :font "Essential PragmataPro-28" :bold nil)))
      `(tab-bar ((,c :font "Essential PragmataPro-28" :background ,bg-main)))
      `(tab-bar-tab ((,c :underline ,green :overline ,green :foreground "#a9c99f" )))
      `(tab-bar-tab-inactive ((,c :bold nil :foreground "#a9c99f")))))

    (set-face-attribute 'font-lock-number-face nil :foreground "#ff9580" :bold nil)
    (set-face-attribute 'font-lock-escape-face nil :foreground "#feacd0")
    (set-face-attribute 'font-lock-function-call-face nil
                        :inherit nil
                        :foreground "#00bcff"
                        :bold nil)
    (set-face-attribute 'font-lock-function-name-face nil
                        :background "#004065"
                        :foreground "#00d3d0")
    (set-face-attribute 'font-lock-builtin-face nil :bold nil))


  ;; Using the hook lets our changes persist when we use the commands
  ;; `ef-themes-toggle', `ef-themes-select', and `ef-themes-load-random'.
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-custom-faces)


  (defun my-ef-themes-mode-line ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
     (custom-set-faces
      ;; `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
      `(mode-line
        ((,c
          :background ,bg-mode-line
          :foreground ,fg-mode-line
          :box (:line-width 1 :color ,fg-dim))))

      `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-mode-line))

(provide 'diego-ui-theme)
