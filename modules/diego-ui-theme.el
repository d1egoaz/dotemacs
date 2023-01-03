(use-package modus-themes
  :config

  (setq modus-themes-italic-constructs t)
  (setq  modus-themes-bold-constructs nil)
  (setq  modus-themes-mixed-fonts t)
  (setq  modus-themes-variable-pitch-ui t)
  (setq  modus-themes-prompts '(italic bold))
  (setq  modus-themes-completions
         '((matches . (extrabold))
           (selection . (semibold text-also))))

  ;; (with-eval-after-load 'vertico (set-face-attribute 'vertico-current nil
  ;;                                                    :inherit 'pulsar-yellow))
  (defun diego--improve-tabs-color ()
    (modus-themes-with-colors
      (custom-set-faces
       `(web-mode-block-delimiter-face ((,c :foreground ,yellow)))
       `(tab-bar-tab ((,c :background ,bg-active)))
       `(tab-bar-tab-inactive ((,c :background ,bg-inactive :foreground ,fg-dim))))))

  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)
  (add-to-list 'modus-themes-common-palette-overrides '(diego fg-main))

  (setq modus-vivendi-palette-overrides '((bg-main "#212121")
                                          (fg-main "#c2c2c2")))

  (modus-themes-select 'modus-vivendi)
  :bind ("<f5>" . #'modus-themes-toggle)
  :hook (modus-themes-after-load-theme-hook . diego--improve-tabs-color))

;; (use-package base16-theme
;;   :after modus-themes
;;   :config
;;   (modus-themes-load-themes)
;;   (setq base16-theme-highlight-mode-line t)
;;   (setq base16-theme-distinct-fringe-background nil)

;;   (set-face-attribute 'tab-bar-tab nil :background "#00422a" :foreground "#9ff0cf") ;; modus-themes-refine-green
;;   (set-face-attribute 'tab-bar-tab-inactive nil :background "#004065" :foreground "#8ae4f2") ;; modus-themes-refine-cyan

;;   (require 'tab-line)
;;   (set-face-attribute 'tab-line nil :inherit 'modus-themes-tab-backdrop)
;;   (set-face-attribute 'tab-line-tab   nil :inherit 'modus-themes-tab-inactive)
;;   (set-face-attribute 'tab-line-tab-current nil  :inherit 'tab-line-tab :foreground "#9ff0cf")
;;   (set-face-attribute 'tab-line-tab-inactive  nil :inherit 'modus-themes-tab-active)
;;   (set-face-attribute 'tab-line-tab-modified   nil :foreground "#7f002f"))

;; (load-theme 'base16-darcula t)
;; (load-theme 'base16-tomorrow-night t)

(provide 'diego-ui-theme)
