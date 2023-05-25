(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs nil)
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-variable-pitch-ui t)
  (setq modus-themes-prompts '(italic bold))
  (setq modus-themes-org-blocks 'tinted-background)
  (setq modus-themes-completions
        '((matches . (extrabold))
          (selection . (semibold text-also))))

  (defun diego--improve-tabs-color ()
    (modus-themes-with-colors
      (custom-set-faces
       `(web-mode-block-delimiter-face ((,c :foreground ,yellow)))
       `(tab-bar-tab ((,c :background ,bg-blue-nuanced :foreground ,blue :overline ,blue :bold nil)))
       `(tab-bar-tab-inactive ((,c :background ,bg-cyan-subtle :overline ,blue :bold nil))))))

  (setq modus-themes-common-palette-overrides modus-themes-preset-overrides-intense)
  ;; (add-to-list 'modus-themes-common-palette-overrides '(diego fg-main))

  (setq modus-vivendi-palette-overrides '((bg-main "#212121")
                                          (fg-main "#c2c2c2")
                                          (comment  "#6a9955")
                                          (constant "#569cd6")
                                          ;; (keyword  "#569cd6")
                                          (fnname "#dcdcaa")
                                          (string "#ce9178")
                                          (variable "#9cdcfe")))

  ;; (let ((class '((class color) (min-colors 89)))
  ;;     (fg0               "#aeafad")
  ;;     (fg1               "#d4d4d4") ; default fg
  ;;     (fg2               "#e8e8e8")
  ;;     (fg3               "#f4f4f4")
  ;;     (fg4               "#fafafa")
  ;;     (bg0               "#111111")
  ;;     (bg1               "#1e1e1e") ; default bg
  ;;     (bg2               "#252526")
  ;;     (bg3               "#333333")
  ;;     (bg4               "#4b474c")
  ;;     (bg-hl             "#264f78")
  ;;     (vc-r              "#a41511")
  ;;     (vc-g              "#4a7f00")
  ;;     (vc-b              "#207fa1")
  ;;     (key2              "#ce9178")
  ;;     (key3              "#9cdcfe")
  ;;     (accent            "#ffffff")
  ;;     (numeric           "#b5cea8")
  ;;     (mode-line-bg      "#007acc")
  ;;     (mode-line-bg-dark "#005aa3")
  ;;     (line-num          "#858585")
  ;;     (line-num-current  "#c6c6c6")
  ;;     (builtin           "#c586c0")
  ;;     (keyword           "#569cd6")
  ;;     (const             "#569cd6")
  ;;     (comment           "#6a9955")
  ;;     (doc               "#ce9178")
  ;;     (doc-alt           "#888888")
  ;;     (func              "#dcdcaa")
  ;;     (str               "#ce9178")
  ;;     (type              "#4ec9b0")
  ;;     (var               "#9cdcfe")
  ;;     (warning           "#f16969")

  ;;     ;; standardized palette
  ;;     (ms-yellow         "#dcdcaa")
  ;;     (ms-bluegreen      "#4ec9b0")
  ;;     (ms-magenta        "#c586c0")
  ;;     (ms-orange         "#ce9178")
  ;;     (ms-lightorange    "#d7ba7d")
  ;;     (ms-red            "#d16969")
  ;;     (ms-green          "#6a9955")
  ;;     (ms-blue           "#569cd6")
  ;;     (ms-lightred       "#f19999")
  ;;     (ms-lightgreen     "#b5cea8")
  ;;     (ms-lightblue      "#9cdcfe")
  ;;     (ms-red-bg         "#551b1e")
  ;;     (ms-green-bg       "#39422a")
  ;;     (ms-blue-bg        "#040e3f")
  ;;     (ms-red-bghl       "#74140f")
  ;;     (ms-green-bghl     "#4b5332")
  ;;     (ms-blue-bghl      "#141e4f"))


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
