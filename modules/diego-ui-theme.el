(use-package modus-themes
  :init
  (setq modus-themes-completions '((t background accented)))
  (setq modus-themes-box-buttons '(variable-pitch flat semilight 0.9))
  (setq modus-themes-diffs nil)
  (setq modus-themes-fringes 'intense) ; {nil,'subtle,'intense}
  (setq modus-themes-headings '((0 . (variable-pitch light (height 2.2)))
                                (1 . (background rainbow overline variable-pitch 1.4))
                                (2 . (background rainbow overline variable-pitch 1.2))
                                (3 . (background rainbow overline variable-pitch 1.1))
                                (4 . (background rainbow overline variable-pitch 1.0))
                                (t . (monochrome))))
  (setq modus-themes-hl-line '(underline-accented))
  (setq modus-themes-links '(faint))
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-mode-line '(padded accented 3d))
  (setq modus-themes-org-blocks '(gray-background))
  (setq modus-themes-paren-match '(intense-bold))
  (setq modus-themes-prompts '(intense-accented))
  (setq modus-themes-region '(bg-only))
  (setq modus-themes-scale-1 1.1)
  (setq modus-themes-markup '(background intense bold))
  (setq modus-themes-scale-2 1.15)
  (setq modus-themes-scale-3 1.21)
  (setq modus-themes-scale-4 1.27)
  (setq modus-themes-scale-5 1.33)
  (setq modus-themes-scale-headings t)
  (setq modus-themes-slanted-constructs t) ; use slanted text (italics) unless it is absolutely necessary, strings and code comments
  (setq modus-themes-subtle-line-numbers t)
  (setq x-underline-at-descent-line t) ; to make the underline not break bottom part of letters, like g
  (setq-default text-scale-remap-header-line t) ; And this is for Emacs28.
  (setq x-underline-at-descent-line t) ; to make the underline not break bottom part of letters, like g
  (setq modus-themes-syntax '(yellow-comments alt-syntax))
  (setq modus-themes-tabs-accented nil)
  ;; default white is too bright
  (setq modus-themes-vivendi-color-overrides '((fg-main . "#c2c2c2")))
  (with-eval-after-load 'vertico (set-face-attribute 'vertico-current nil
                                                     :inherit 'pulsar-yellow))
  (defun diego--improve-tabs-color ()
    (set-face-attribute 'tab-bar-tab nil
                        :inherit 'modus-themes-refine-green)
    (set-face-attribute 'tab-bar-tab-inactive nil
                        :inherit 'modus-themes-refine-cyan))
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config (modus-themes-load-vivendi)
  (diego--improve-tabs-color)
  :bind ("<f5>" . #'modus-themes-toggle)
  :hook (modus-themes-after-load-theme-hook . diego--improve-tabs-color))

(provide 'diego-ui-theme)
