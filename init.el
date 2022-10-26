;;; init.el --- Init File                            -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Diego Alvarez

;; Author: Diego Alvarez <diego.canada@icloud.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; ** How I Install Emacs

;;    #+begin_example sh
;; brew install libxml2 gcc libgccjit
;; ./autogen.sh
;; ./configure --with-native-compilation=aot --without-compress-install --with-xwidgets
;; gmake install
;; The Emacs.app App bundle should now be in the nextstep/ directory. Simply copy it to the /Applications folder to use it.
;; #brew install emacs-plus@28 --with-no-frame-refocus --with-native-comp
;; #ln -s /opt/homebrew/opt/emacs-plus@29/Emacs.app /Applications
;;    #+end_example

;; ** Lexical Binding

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Lexical-Binding.html
;; It eliminates the problem of which variables lambda-expressions use (when they
;; attempt to use variables from their surrounding context), and much easier for
;; the compiler to optimize, because it doesn't need to worry about variables
;; escaping their lexical context.

(setq user-full-name "Diego Alvarez")
(setq user-mail-address "diego.canada@icloud.com")

;; ** Emacs Concepts

;; *** Functions

;; In Emacs, every user interaction is a function execution. There are two types of functions in Emacs:

;; - Normal functions ::
;;   These are like functions in other programming languages, and are used for implementing features in
;;   Emacs. Users do not need to care about these functions, unless they want to implement something or
;;   modify an existing implementation.
;; - Commands ::
;;   Commands are like functions, but interactive. It means, commands are features provided to users
;;   and users directly use them.

;; *** Basic motion commands in Emacs

;; Some of the commands don't work on evil mode, but it's a good idea to know how
;; to move on Emacs when evil is not available.

;; | Keymap | Command                             |
;; |--------+-------------------------------------|
;; | C-f    | Move forward one char               |
;; | C-b    | Move backward one char              |
;; |--------+-------------------------------------|
;; | C-p    | Move upward one line                |
;; | C-n    | Move downward one line              |
;; | C-a    | Move to beginning of line           |
;; | C-e    | Move to end of line                 |
;; |--------+-------------------------------------|
;; | M-f    | Move forward one word               |
;; | M-b    | Move backward one word              |
;; |--------+-------------------------------------|
;; | C-v    | Scroll forward one screen           |
;; | M-v    | Scroll backward one screen          |
;; |--------+-------------------------------------|
;; | M-a    | Move to the beginning of a sentence |

;; *** Mode line

;; The mode line is the empty area below the buffer. It has useful summary
;; information about the buffer shown in the window.

;; *** Minibuffer

;; Minibuffer is the small area at the bottom of your Emacs screen.

;; **** Minibuffer completion uses:

;; - completing-read :: to define what the completion UI looks like and how it behaves.
;; - completing-styles :: to define how completion filter/sorts results.

;; *** Echo area

;; Minibuffer can be used for output as well. The echo area is used for displaying
;; messages. Both Minibuffer and Echo Area, although serve different purposes,
;; share the same physical space.

;; *** Frames

;; An application window in an operating system is called a Frame in Emacs.

;; *** Window

;; Emacs can split your frame area into multiple smaller areas. Each such area is called a window.

;; *** Tutorial

;; - https://tuhdo.github.io/emacs-tutor.html

;; * Package Manager

;; ** straight.el

;; *=straight= is configured and installed in early-init.el.*

;; straight.el operates by cloning Git repositories and then symlinking files into
;; Emacs' load path.

;; =straight-use-package= package name is a *symbol* not a string.
;; =straight-visit-package-website= to visit package URL.

;; *** Update Packages

;; Update packages (pull in changes, then freeze), this creates
;; =~/.config/emacs/straight/versions/default.el=, this lockfile should be checked in.

;; #+begin_example elisp
;; (straight-pull-all)
;; (straight-freeze-versions)
;; (straight-remove-unused-repos)
;; #+end_example

;; *** Rollback/Re-Install Packages

;; Read version lockfile and restore package versions to those listed.

;; #+begin_example elisp
;; (straight-thaw-versions)
;; #+end_example

;; ** use-package.el

;; *=use-package= is configured and installed in early-init.el.*

;; Package =use-package= provides a handy macro by the same name which is
;; essentially a wrapper around =with-eval-after-load= with a lot of handy
;; syntactic sugar and useful features.

;; A common use-package declaration looks like this:

;; #+begin_example elisp
;; (use-package <package-name>
;;    ;; The :init configuration is always executed (Not lazy)
;;     :init
;;    ;; commands to auto load
;;     :commands
;;    ;; Configure other variables and modes in the :config section,
;;    ;; after lazily loading the package.
;;     :config
;;     ;; configure hooks
;;     :hook
;;      ;; key bindings for this package>
;;     :bind)
;; #+end_example

(setq use-package-hook-name-suffix nil)

;; ** Keep .emacs.d clean (no-littering.el)

(use-package no-littering
  :config
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; * General Emacs Base Settings

;; ** Garbage collection

;; Enforce a sneaky Garbage Collection strategy to minimize GC interference with
;; user activity. During normal use a high GC threshold is set.  When idling GC is
;; triggered and a low threshold is set.

;; This is important as Emacs just dies trying to collect GC due to the huge
;; initial GC threshold in =early-init.el=.

(use-package gcmh
  :config
  (gcmh-mode 1))

;; ** Disable startup messages and bell

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; ** Authinfo

(setq auth-sources '("~/.authinfo.gpg"))

;; ** Pinentry

;; Emacs can be prompted for the PIN of GPG private keys.

;; reload gpg agent: =gpg-connect-agent reloadagent /bye=

;; First, edit the gpg-agent configuration to allow loopback pinentry mode:
;; =~/.gnupg/gpg-agent.conf=
;; #+begin_example
;; allow-loopback-pinentry
;; #pinentry-program /opt/homebrew/bin/pinentry-mac
;; #+end_example

;; =~/.gnupg/gpg.conf=
;; #+begin_example
;; pinentry-mode loopback
;; #+end_example

(setq epg-pinentry-mode 'loopback)

;; ** Disable exit confirmation
(setq confirm-kill-emacs nil)


;; ** Disable lock files

;; Disables .#file.ext creation.

;;#+begin_src elisp
(setq create-lockfiles nil)


;; ** Enable files backup

(setq delete-by-moving-to-trash t)
;; creating backups:
(setq auto-save-default t)
(setq backup-by-copying t)
(setq delete-old-versions -1)
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq version-control t)

;; for the ` and , see:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
;; The special marker ‘,’ inside of the argument to backquote indicates a value that isn’t constant.
(setq backup-directory-alist `(("." . ,(concat no-littering-var-directory "backup"))))


;; ** Kill ring not save duplicates
;; Remove duplicates in the kill ring.
(setq kill-do-not-save-duplicates t)


;; ** Use short answers y/n
;; y-or-n-p  uses shorter answers "y" or "n".
(setq use-short-answers t) ; new in Emacs28

;; ** Server mode
;; Start the Emacs server from this instance so that all =emacsclient= calls are routed here.
(add-hook 'after-init-hook #'server-start)

;; ** Default emacs client

;; Fix `with-editor: Cannot determine a suitable Emacsclient` issue.
(setq-default with-editor-emacsclient-executable "emacsclient")

;; ** Increase read output from processes

;; Increase how much is read from processes in a single chunk (default is 4kb). LSP is improved by increasing this value.
(setq read-process-output-max (* 1024 1024)) ; 1mb

;; ** Recent.el
(use-package recentf
  :config
  (setq recentf-max-menu-items 1000)
  (setq recentf-max-saved-items 1000)
  (setq recentf-auto-cleanup nil)
  (recentf-mode 1))


;; ** Configure PATH on macOS

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "EMACS_ADDITIONAL_DIR" "KUBECONFIG" "FZF_DEFAULT_OPTS" "FZF_DEFAULT_COMMAND" "FZF_CTRL_T_COMMAND"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; ** Tramp

(setq tramp-verbose 2)


;; * UI - Let's make Emacs look a little better.

;; ** Disable toolbars and scrollbars

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode 1) ; I do like to have the menu-bar available to use when I break Emacs :D


;; ** Increase fill column width

(setq-default fill-column 100)


;; ** Fonts

;; *** Setting The Font Face

;; Fontconfig pattern, fontname[-fontsize][:name1=values1][:name2=values2]...
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
(add-to-list 'default-frame-alist '(font . "Iosevka SS08-14:weight=medium"))
(add-to-list 'default-frame-alist '(undecorated . t))

(setq-default line-spacing 1) ; needs to be changed for some fonts

(set-face-attribute 'fixed-pitch nil :font "-*-Iosevka SS08-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-face-attribute 'fixed-pitch nil :font "Cascadia Code-14")
(set-face-attribute 'variable-pitch nil :font "SF Pro Text-14")
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)


;; *** Configure emoji font 😎

(set-fontset-font t 'emoji
                  '("Apple Color Emoji" . "iso10646-1") nil 'prepend)

;; *** Zooming In and Out

;; Command/Super plus =/- for zooming in/out.

(global-set-key (kbd "s-=") #'text-scale-increase)
(global-set-key (kbd "s--") #'text-scale-decrease)

;; *** pixel-scroll-precision
;; Helps when scrolling images, as Emacs treats pictures as a single characters.
(pixel-scroll-precision-mode 1)


;; ** Theme (modus-themes.el)

(use-package modus-themes
  :init
  (setq modus-themes-completions '((t background accented)))
  (setq modus-themes-box-buttons '(variable-pitch flat semilight 0.9))
  (setq modus-themes-diffs nil)
  (setq modus-themes-fringes 'intense) ; {nil,'subtle,'intense}
  (setq modus-themes-headings
        '((0 . (variable-pitch light (height 2.2)))
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

  (with-eval-after-load 'vertico
    (set-face-attribute 'vertico-current nil :inherit 'pulsar-yellow))

  (defun diego--improve-tabs-color ()
    (set-face-attribute 'tab-bar-tab nil :inherit 'modus-themes-refine-green)
    (set-face-attribute 'tab-bar-tab-inactive nil :inherit 'modus-themes-refine-cyan))
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi)
  (diego--improve-tabs-color)
  :bind ("<f5>" . #'modus-themes-toggle)
  :hook (modus-themes-after-load-theme-hook . diego--improve-tabs-color))


;; ** Maximize the Emacs Frame on Startup

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; * Buffers

;; ** Persistent Scratch buffers

;; *** Always start with a custom scratch buffer
(setq initial-buffer-choice "~/scratch/*scratch*")

;; *** Open the scratch buffers when opening emacs.

(defun diego--init-scratch-buffers ()
  (tab-bar-rename-tab "|scratch|") ;; create initial tab
  (kill-buffer "*scratch*") ;; in case that for some reason it opened the emacs scratch buffer and not my version
  (find-file "~/scratch/*scratch*" t)
  (with-current-buffer "*scratch*" ; Protect scratch buffer against accidental kill
    (emacs-lock-mode 'kill)))

(add-hook 'after-init-hook #'diego--init-scratch-buffers)


;; *** Custom scratch buffers
(setq diego--scratch-mode-list '(("org-mode" . ".org")
                                 ("emacs-lisp" . ".el")
                                 ("text-mode" . ".txt")
                                 ("markdown" . ".md")))

(defun diego--scratch-buffer-query-modes ()
  (alist-get
   (completing-read "Mode: " diego--scratch-mode-list)
   diego--scratch-mode-list nil nil 'equal))

    ;;;###autoload
(defun diego/make-scratch (ext)
  "Get a scratch buffer for with the extension EXT and a random name.
    When called interactively with a prefix arg, prompt for the mode."
  (interactive (list (diego--scratch-buffer-query-modes)))
  (let* ((name (concat (make-temp-name (format "*scratch-")) ext)))
    (find-file (format "~/scratch/%s" name))
    (with-current-buffer (get-buffer-create name)
      (save-excursion
        (insert (format "Scratch buffer for: %s\n\n" ext))
        (goto-char (point-min))
        (comment-region (point-at-bol) (point-at-eol)))
      (forward-line 2))))

(defun diego/make-new-scratch-buffer-go-babel ()

  "New temporary scratch buffer with a random name with go-babel enabled."
  (interactive)
  (diego/make-scratch ".org")
  (insert "
    \#+begin_src go
    package main
    import \"fmt\"
    func main() {
        fmt.Println(\"hello d1egoaz\")
    }
    \
    "))


;; * Mode line

;; ** Make it compact
(setq mode-line-compact t)


;; ** Format

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;;"[" (:eval (diego/current-tab-name)) "]"
                ;; " "
                ;; (:eval (if (eq (buffer-local-value 'major-mode (current-buffer)) 'kubel-mode)
                ;;            (kubel-current-state)))
                mode-line-buffer-identification  " "
                ;; default-directory
                mode-line-position
                minions-mode-line-modes
                ;; (vc-mode vc-mode) " "
                ;; mode-line-misc-info
                ;; mode-line-mule-info
                ;; mode-line-client
                ;; mode-line-modified
                ;; mode-line-remote
                mode-line-frame-identification
                mode-line-end-spaces))


;; ** Disable evil mode line
(setq evil-mode-line-format nil)

;; ** Hide mode line minor modes

(use-package minions
  :config
  (setq minions-mode-line-lighter "+")
  (setq minions-direct '(flymake-mode lsp-mode compilation-shell-minor-mode diego/dedicated-mode))
  (minions-mode 1))

;; ** Show current command in the mode line (keycast.el)

;; Keycast mode shows the current command and its key or mouse binding in the mode
;; line, and updates them whenever another command is invoked.

(use-package moody) ; required by keycast window predicate

(use-package keycast
  :after moody
  :init
  (setq keycast-insert-after 'mode-line-misc-info)
  :config
  (setq keycast-window-predicate 'moody-window-active-p)
  (setq keycast-remove-tail-elements nil) ; leave mode line alone

  ;; copied from Prot
  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))

  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll
                   ;; added these additional events
                   lsp-ui-doc--handle-mouse-movement
                   ignore))
    (add-to-list 'keycast-substitute-alist `(,event nil)))

  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))

  ;; (add-to-list 'global-mode-string '("" mode-line-keycast " "))
  (set-face-attribute 'keycast-key nil :height 1.0)
  (set-face-attribute 'keycast-command nil :height 0.5)
  (keycast-mode 1))


;; ** Show column number

(column-number-mode 1) ; Show column number next to line number in mode line
(setq mode-line-position-column-line-format '(" (Ln:%l, Col:%c)"))


;;** Show date and time

;;#+begin_src elisp
(use-package time
  :init
  (setq display-time-format "%Y-%m(%b)-%d(%a) %I:%M%p %Z")
  (setq display-time-interval 17)
  (setq display-time-default-load-average nil))


;;* Windows

;;** Jump to windows (ace-window.el)

(use-package ace-window
  :after embark
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t)

  ;;** Window rules

  ;; The =display-buffer-alist= is a rule-set for controlling the placement of windows.

  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-Display-Action-Alists.html

  (load (expand-file-name "diego-workspaces.el" user-emacs-directory))


  ;; read: https://www.masteringemacs.org/article/demystifying-emacs-window-manager
  ;; Great for all those annoying popups where you only care about the contents when something goes wrong.
  ;; (add-to-list 'buffer-display-alist
  ;;      '("\\*compilation\\*" display-buffer-no-window
  ;;         (allow-no-window . t)))

  (defun diego--debug-buffer-alist (b a)
    (message "b:%s" b)
    (print a)
    nil)

  (setq display-buffer-alist
        `(
          ;; ↑ top side window
          ("\\*\\(Messages\\|world-clock\\|Backtrace\\|Warnings\\|Compile-Log\\|Flymake log\\)\\*"
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . top)
           (window-height . 0.3)
           (dedicated . t)
           (preserve-size . (t . t)))
          ;;️↓ bottom side window
          ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))
          ("\\*Flycheck errors\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.3))
          ((derived-mode . reb-mode) ; M-x re-builder
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 4) ; note this is literal lines, not relative
           (dedicated . t)
           (preserve-size . (t . t)))
          ((derived-mode . compilation-mode)
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.3))
          ;; ← left side window
          ((or . ((derived-mode . help-mode)
                  (derived-mode . apropos-mode)
                  (derived-mode . helpful-mode)))
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . left)
           (window-width . 0.40))
          ;; → right side window
          ("\\*\\(vterm\\|vterm-project\\|VC-history\\).*"
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . right)
           (window-width . 0.50))
          ("\\*Ilist\\*"
           (display-buffer-reuse-mode-window display-buffer-in-side-window)
           (side . right)
           (window-width . 0.20))
          ;; below current window
          ("\\*\\(Calendar\\|Org todo\\)\\*"
           (display-buffer-reuse-window display-buffer-below-selected)
           (window-height . fit-window-to-buffer))
          ;; ***************************
          ;; Workspaces (dedicated tabs
          ;; ***************************
          ("\\*diego/vterm\\*"
           (display-buffer-in-tab)
           (tab-name . "|vterm|"))
           ;;;; Kubel
          ("\\*kubel-process.*"
           (display-buffer-in-tab display-buffer-in-side-window)
           (tab-name . "|kubel|")
           (side . bottom)
           (window-height . 0.2)
           (slot . 0))
          ("\\*kubel stderr\\*"
           (display-buffer-in-tab display-buffer-in-side-window)
           (tab-name . "|kubel|")
           (side . bottom)
           (window-height . 0.2)
           (slot . 1)
           (window-parameters . ((no-other-window . t))))
          ("\\*kubel resource.*"
           (display-buffer-in-tab display-buffer-in-side-window)
           (tab-name . "|kubel|")
           (side . right)
           (window-width . 0.5))
          ("\\*\\(kubectl\\|kubel\\).*"
           (display-buffer-in-tab)
           (tab-name . "|kubel|")
           (dedicated . t))
           ;;;; Elfeed
          ("\\*elfeed-search\\*"
           (display-buffer-in-tab)
           (tab-name . "|elfeed|"))
          ("\\*elfeed-entry\\*"
           (display-buffer-in-tab display-buffer-in-side-window)
           (tab-name . "|elfeed|")
           (side . bottom)
           (window-height . 0.7))
           ;;;; Scratch buffers
          ("\\*scratch.*"
           (display-buffer-in-tab)
           (tab-name . "|scratch|"))
          ("\\*\\(straight-process\\|Async-native-compile-log\\)\\*"
           (display-buffer-in-tab)
           (tab-name . "|general|"))
          ;;; Automatic workspaces-tabs management
          ;; Every buffer visiting a file goes automatically to a tab given by the root project.
          ;; idea from https://emacs.stackexchange.com/a/64486
          (diego/workspaces-validate-buffer
           (display-buffer-in-tab)
           (tab-name . diego/workspaces-name-for-buffer))
          ;; end display-buffer-alist elements
          ))

  ;; (setq display-buffer-alist nil)

  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  ;; (setq even-window-sizes nil)     ; avoid resizing
  (setq window-sides-vertical nil)
  ;; apply rules to all including manual switching
  (setq switch-to-buffer-obey-display-actions t)
  ;; (setq switch-to-prev-buffer-skip t)
  (setq switch-to-prev-buffer-skip nil)

  (setq split-height-threshold nil)
  (setq split-width-threshold nil)

  (add-hook 'help-mode-hook #'visual-line-mode)
  (add-hook 'custom-mode-hook #'visual-line-mode)
  (define-key global-map (kbd "<f6>") #'window-toggle-side-windows)


  ;; elisp
  (defun diego/split-window-horizontally-3 ()
    (interactive)
    (delete-other-windows)
    (split-window-horizontally)
    (split-window-horizontally)
    (balance-windows)
    (other-window -1))

  (defun diego/follow-mode-3 ()
    (interactive)
    (diego/split-window-horizontally-3)
    (follow-mode 1))

  ;;** winmove.el

  ;;#+begin_src elisp(use-package windmove
  :straight (:type built-in)
  :config
  (windmove-install-defaults nil '(meta)
                             '((windmove-display-left ?h)
                               (windmove-display-right ?l)
                               (windmove-display-up ?k)
                               (windmove-display-down ?j)
                               (windmove-display-same-window ?m)))
  (windmove-install-defaults nil '(super)
                             '((windmove-left ?h)
                               (windmove-right ?l)
                               (windmove-up ?k)
                               (windmove-down ?j))))


;;* Workspaces/tabs (tab-bar.el)

(use-package tab-bar
  :general
  (general-nmap
    "gt"  #'tab-next
    "gT"  #'tab-recent)
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice nil)
  (setq tab-bar-show t)
  (setq tab-bar-tab-hints nil) ; don't show numbers

  ;; close tab and project
  (defun diego--close-tab-for-project (tab _bool)
    (let ((name (alist-get 'name tab)))
      (if (string= "*kubel*" name)
          (kill-matching-buffers "\\*kubel" nil t)
        (when (project-current)
          (project-kill-buffers t)))
      (message "Project %s closed." name)))
  (setq tab-bar-tab-pre-close-functions '(diego--close-tab-for-project))

  (defun prot-tab-format-evil ()
    "Format `evil-mode-line-tag for the tab bar."
    `((global menu-item ,evil-mode-line-tag ignore)))

  (defun prot-tab-format-modified ()
    "Format `mode-line-modified' for the tab bar."
    `((global menu-item ,(string-trim-right (format-mode-line mode-line-modified)) ignore)))

  (defun diego--tab-insert-icon (name)
    `((global menu-item ,(propertize (all-the-icons-octicon name) 'face 'modus-themes-refine-yellow) ignore)))

  (defun diego-tab-format-vc ()
    "Format VC status for the tab bar."
    `((global menu-item ,(propertize (concat " " (all-the-icons-octicon "git-branch") vc-mode " ") 'face 'modus-themes-refine-yellow) ignore)))

  (defun diego-tab-format-buffer-id ()  ;
    "Buffer true name for files or just the buffer name."
    (ignore-errors
      `((global menu-item ,(propertize
                            (concat " " (all-the-icons-icon-for-buffer)
                                    (if (and buffer-file-truename (not (file-remote-p buffer-file-truename)) (diego/current-project-name))
                                        (concat " File: ["
                                                (if (string-prefix-p (diego/current-project-name) buffer-file-truename)
                                                    (car (split-string  buffer-file-truename (diego/current-project-name) t nil))
                                                  buffer-file-truename) "] ")
                                      (concat " Buffer: [" (buffer-name) "] ")))
                            'face 'modus-themes-subtle-blue) ignore))))

  (defun diego-tab-format-keycast ()
    "Format `mode-line-modified' for the tab bar."
    `((global menu-item ,(format-mode-line keycast-mode-line) ignore)))

  (defun diego-tab-format-line-break ()
    "Format a line break to simulate another row for the tab bar."
    `((global menu-item "\n" ignore)))

  (setq tab-bar-separator "/")

  (setq tab-bar-format
        '(mode-line-front-space
          prot-tab-format-modified
          prot-tab-format-evil
          tab-bar-format-tabs ;; tab-bar-format-tabs-groups ; remove as it duplicates the tabs
          diego-tab-format-keycast
          diego-tab-format-line-break
          diego-tab-format-vc
          diego-tab-format-buffer-id
          tab-bar-format-align-right
          tab-bar-format-global
          ))

  ;; tab-bar-format-global
  ;; prot-tab-format-space-single))

  (defun diego/tab-bar-list-names ()
    (mapcar (lambda (tab)
              (alist-get 'name tab))
            (tab-bar-tabs)))

  (setq tab-bar-mode t)
  (global-tab-line-mode 1)
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))


;;* Files

;;** Don't prompt for confirmation when we create a new file or buffer.

(setq confirm-nonexistent-file-or-buffer nil)


;;** Create missing directories when using find file

;;Create missing directories when we open a file that doesn't exist under a directory tree that may not exist.

;;#+begin_src elisp
(defun diego/my-create-non-existent-directory ()
  "Automatically create missing directories when creating new files."
  (unless (file-remote-p buffer-file-name)
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent-directory))
           (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory))
           (progn (make-directory parent-directory 'parents)
                  t)))))
(add-to-list 'find-file-not-found-functions #'diego/my-create-non-existent-directory)


;;** Follow symlinks when opening files.

(setq vc-follow-symlinks t)
(setq find-file-visit-truename t)


;;** Disable the warning X and Y are the same file
;; Which normally appears when you visit a symlinked file by the same name.

(setq find-file-suppress-same-file-warnings t)


;;** Disable file changed on disk messages

;; Turn the delay on auto-reloading from 5 seconds down to 1 second.  We have to do this before turning
;; on =auto-revert-mode= for the change to take effect.

(use-package autorevert
  :straight (:type built-in)
  :config
  ;; Revert Dired and other buffers
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-interval 1)
  (setq revert-without-query '(".*")) ; disables prompt
  (global-auto-revert-mode 1))


;;** Restore cursor to last visited place in a file

;; This means when you visit a file, point goes to the last place where it was when you previously
;; visited the same file.

(use-package saveplace
  :straight (:type built-in)
  :config
  (setq-default save-place t)
  (save-place-mode 1))


;;** Save Some Buffers default to project root

;;#+begin_src elisp
(setq save-some-buffers-default-predicate 'save-some-buffers-root)


;;** Make Script Files Executable Automatically

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)


;;* Editor

;;** Default coding system

(set-default-coding-systems 'utf-8)


;;** New line at EOF
;; Add a newline automatically at the end of the file.
(setq require-final-newline t)

;;** Display line numbers and truncated lines

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes.
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(setq-default truncate-lines t)
(setq-default truncate-partial-width-windows nil)
(setq-default visual-line-mode nil)

(defun my-truncate-lines-disable ()
  (let ((inhibit-message t))
    (setq truncate-lines t)))

;; (global-visual-line-mode nil)
(add-hook 'prog-mode-hook #'my-truncate-lines-disable)


;;** Highlight current line
(global-hl-line-mode 1)


;;** Highlight TODO, NOTES, etc.

(add-hook 'find-file-hook
          (lambda() (highlight-phrase "\\(BUG\\|FIXME\\|TODO\\|NOTE\\):")))

;;** Avoid performance issues with long lines

;; When the lines in a file are so long that performance could suffer to an unacceptable degree, we say
;; "so long" to the slow modes and options enabled in that buffer, and invoke something much more basic
;; in their place.

(global-so-long-mode 1)


;;** Indentation, spaces, and tabs
;; Favor spaces over tabs.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


;; Make =tabify= and =untabify= only affect indentation. Not tabs/spaces in the middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;;** Whitespace cleanup on buffer save

(use-package whitespace
  :straight (:type built-in)
  :hook
  (before-save-hook . whitespace-cleanup))

;;** Enable narrowing functions

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;* Let's Be Evil (evil-*.el)

;; - Cutting and Pasting ::
;; In emacs, cutting is called killing. Pasting is called yanking.

;; - Point and Mark ::
;; The point refers to the cursor. The mark refers to the other side of a selected region (the “active region”).

;; - Guides ::
;; https://github.com/noctuid/evil-guide

;; [[https://github.com/emacs-evil/evil][evil]] is a 'vi' layer for Emacs.

(use-package evil
  :init
  (setq evil-search-module 'isearch)
  (setq evil-kill-on-visual-paste nil) ; don't add the replaced text to the kill ring
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil) ; so we can use evil-collection
  (setq evil-want-minibuffer nil)
  (setq evil-want-C-u-delete nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-delete t) ; when insert mode
  (setq evil-want-Y-yank-to-eol t) ; behave like y$
  ;; (setq evil-undo-system 'undo-redo) ; default to natively Emacs 28
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-fine-undo t)
  ;; search for the symbol instead of the word when using `*`
  (setq evil-symbol-word-search t)
  :config
  ;; set up some basic equivalents for vim mapping functions. This creates
  ;; global key definition functions for the evil states.
  (general-evil-setup t) ; https://github.com/noctuid/general.el#vim-like-definers
  (evil-put-command-property #'evil-goto-definition :jump t)

  ;; n=nzz
  (defun diego--my-center-line (&rest _)
    (evil-scroll-line-to-center nil))
  (advice-add #'evil-search-next :after #'diego--my-center-line)
  ;; N=Nzz
  (advice-add #'evil-search-previous :after #'diego--my-center-line)

  ;; keep J centered
  (evil-put-command-property #'evil-join :move-point nil)
  (defun diego--my-save-position-line (fn &rest args)
    (save-excursion (apply fn args)))
  (advice-add #'evil-join :around #'diego--my-save-position-line)

  ;; from: https://macs.stackexchange.com/a/48721
  (defun my/evil-shift-right ()
    (interactive)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun my/evil-shift-left ()
    (interactive)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))

  (evil-mode 1))


;;** evil-collection.el

;; [[https://github.com/emacs-evil/evil-collection][evil-collection]] are Evil bindings for the parts of Emacs that Evil does not cover properly by default, such as
;; help-mode, M-x calendar, Eshell and more. Some bindings don't make sense, so I'm just enabling it per mode.

;; Motion ([, ], {, }, (, ), gj, gk, C-j, C-k)

(use-package evil-collection
  :after evil
  :commands (evil-collection-dired-setup evil-collection-magit-setup)
  :init
  (setq evil-collection-company-use-tng nil) ; I don't want that completion experience
  (setq evil-collection-mode-list nil) ; I don't want surprises, I'll enable it manually by mode
  (setq evil-collection-key-blacklist '("SPC" "SPC m" "C-SPC" "M-SPC" "gd" "gf" "K" "gr" "gR" "[" "]" "gz" "<escape>"))
  (setq evil-collection-setup-minibuffer nil) ; don't setup Vim style bindings in the minibuffer.
  (setq evil-collection-setup-debugger-keys nil)
  (setq evil-collection-calendar-want-org-bindings t)
  :config
  ;; https://github.com/emacs-evil/evil-collection/blob/master/modes/
  (evil-collection-init '(
                          calendar comint company compile consult
                          diff-mode dired docview
                          ediff eglot elfeed elisp-mode elisp-refs eshell
                          flycheck flymake
                          go-mode
                          help helpful
                          ibuffer info imenu imenu-list
                          magit ocurr popup
                          vc-annotate vc-dir vc-git
                          vterm wgrep which-key xref)))

(with-eval-after-load 'dired (evil-collection-dired-setup))
(with-eval-after-load 'magit (evil-collection-magit-setup))


;;** goto-chg.el
;; | Keymap | Command                  |
;; |--------+--------------------------|
;; | g;     | goto-last-change         |
;; | g,     | goto-last-change-reverse |

(use-package goto-chg :after evil)


;;** evil-args.el

;; Motions and text objects for delimited arguments.

;; For example, =cia~ (~ia= inner arg) transforms:
;; #+begin_example
;; function(ar|g1, arg2, arg3)
;; function(|, arg2, arg3)
;; #+end_example

;; =daa= (=aa= outer arg) transforms:
;; #+begin_example
;; function(ar|g1, arg2, arg3)
;; function(|arg2, arg3)
;; #+end_example

(use-package evil-args
  :after evil
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
  ;; bind evil-forward/backward-args
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)
  ;; bind evil-jump-out-args
  ;; (define-key evil-normal-state-map "K" 'evil-jump-out-args)

  (setq evil-args-delimiters '("," ";")); include space to use in lisp

  (defun diego--fix-evil-args-lisp ()
    (make-local-variable 'evil-args-delimiters)
    (setq evil-args-delimiters " "))

  :bind (:map
         evil-inner-text-objects-map
         ("a" . #'evil-inner-arg)
         :map
         evil-outer-text-objects-map
         ("a" . #'evil-outer-arg)
         :map
         evil-normal-state-map
         ("L" . #'evil-forward-arg)
         ("H" . #'evil-backward-arg)
         :map
         evil-motion-state-map
         ("H" . #'evil-backward-arg)
         ("L" . #'evil-forward-arg))
  :hook ((emacs-lisp-mode-hook . diego--fix-evil-args-lisp)
         (org-mode-hook . diego--fix-evil-args-lisp)))


;;** evil-commentary.el

;; evil-commentary is an Emacs package for evil-mode that intends to make it easy to comment out (lines of) code:

;; | Keymap | Command                             |
;; |--------+-------------------------------------|
;; | gcc    | comment out a line                  |
;; | gc~    | comments out the target of a motion |
;; | gcap   | comment out a paragrah              |
;; | gc     | comment out selection               |

(use-package evil-commentary
  :straight (:build (autoloads native-compile))
  :after evil
  :config
  (evil-commentary-mode 1))


;;** evil-exchange.el

;; Easy text exchange operator for Evil.

;; On the first use, define (and highlight) the first {motion} to exchange. On the
;; second use, define the second {motion} and perform the exchange.

;; =gx= can also be used from visual mode, which is sometimes easier than coming up with the right {motion}

;; | Keymap | Command                   |
;; |--------+---------------------------|
;; | gx     | evil exchange             |
;; | .      | repeat motion to exchange |
;; | gX     | evil exchange cancel      |

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))


;;** evil-goggles.el

;;#+begin_src elisp
(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-pulse t)
  (custom-set-faces
   '(evil-goggles-default-face ((t (:inherit 'modus-themes-active-yellow)))))
  (setq evil-goggles-duration 0.3)
  (evil-goggles-mode))

;;** evil-snipe.el

;;#+begin_src elisp
(use-package evil-snipe
  :after evil
  :config
  (setq evil-snipe-scope 'whole-visible)
  (evil-snipe-mode)
  (evil-snipe-override-mode)
  :hook ((magit-mode-hook . turn-off-evil-snipe-override-mode)))

;;** evil-surround.el

;; Add/change surrounding to text objects.

;; | Keymap         | Command                         |
;; |----------------+---------------------------------|
;; | S<textobject>  | Add surrounding in region       |
;; | ys<textobject> | Add surrounding in normal state |
;; | ds<textobject> | Delete surrounding              |

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;;** evil Tuning

;;*** Change cursor color evil-mode

(setq evil-insert-state-cursor '((bar . 2) "#ff00ff"))
(setq evil-normal-state-cursor '(box "#ff00ff"))

;;*** Stay on the original character when leaving insert mode

(setq evil-move-cursor-back nil)
(setq evil-shift-round nil)

;;*** Make magit commit buffer start in insert mode

(add-hook 'with-editor-mode-hook #'evil-insert-state)


;;* Project Management

;;** Git

;; https://github.com/magit/magit

;; A git client for Emacs.
;; C-t to turn any magit buffer into text-mode.

;; Keybindings: https://github.com/emacs-evil/evil-collection/tree/master/modes/magit

;;*** magit.el

;; Keys:
;; https://github.com/emacs-evil/evil-collection/blob/master/modes/magit/evil-collection-magit.el#L280-L309

(use-package magit
  :general
  (general-nvmap :keymaps 'magit-status-mode-map
    "zt" #'evil-scroll-line-to-top
    "zz" #'evil-scroll-line-to-center
    "zb" #'evil-scroll-line-to-bottom
    "gr" #'magit-refresh)
  (general-nvmap :keymaps 'magit-log-mode-map
    "zt" #'evil-scroll-line-to-top
    "zz" #'evil-scroll-line-to-center
    "zb" #'evil-scroll-line-to-bottom
    "gr" #'magit-refresh)

  (general-nvmap
    :keymaps 'magit-status-mode-map
    :prefix ","
    "b"  '(:ignore t :which-key "branch")
    "bb" #'(diego/git-create-branch-from-origin-master :which-key "branch of origin/master")
    "bm" #'(diego/git-create-branch-from-origin-main :which-key "branch of origin/main")
    "p" '(:ignore t :which-key "pr")
    "pc"  #'forge-create-pullreq
    "pC" #'diego/checkout-gh-pr
    "o" #'diego/fetch-and-rebase-onto-origin-master
    "v" #'diego/visit-pull-request-url)

  :config
  (setq magit-diff-refine-hunk t) ; show granular diffs in selected hunk
  (setq magit-save-repository-buffers nil) ; Don't autosave repo buffers
  ;; Don't display parent/related refs in commit buffers; they are rarely
  ;; helpful and only add to runtime costs.
  (setq magit-revision-insert-related-refs nil)
  (setq magit-diff-refine-ignore-whitespace nil)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  ;; (setq magit-display-buffer-function #'display-buffer) to use window rules

  (setq magit-repository-directories
        '(
          ("~/src/github.com/Shopify" . 2)
          ("~/code/" . 2)
          ("~/dotfiles/" . 1)))

  (setq magit-bury-buffer-function 'magit-restore-window-configuration)

  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))

  (load (expand-file-name "diego-magit.el" user-emacs-directory)))


;;*** transient.el
;; Package `transient' is the interface used by Magit to display popups.
;; TODO remove use package as it's now part of Emacs

(use-package transient
  :config
  ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
  ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
  ;; for discussion.
  (transient-bind-q-to-quit)
  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one))


;;*** git-link.el

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t))


;;*** forge.el

(use-package forge
  :commands forge-create-pullreq)


;;** project.el

(use-package project
  :straight (:type built-in)
  :config
  (load (expand-file-name "diego-project.el" user-emacs-directory))

  (defun diego--open-readme-and-vterm ()
    (interactive)
    (diego/open-project-readme)
    (diego/vterm-project))

  (setq project-switch-commands '((project-find-file "Find file" ?f)
                                  (diego/open-project-readme "README.md" ?.)
                                  (consult-ripgrep "Search" ?s)
                                  (project-dired "Dired" ?d)
                                  (diego/open-project-magit "Git status" ?g)
                                  (diego/consult-buffer-for-project "Recent project buffer" ?R)
                                  (project-shell-command "Shell command" ?!)
                                  (diego--open-readme-and-vterm "Vterm project" ?v))))



;;* Completion

;;** Preserve history with savehist-mode

;; Run =(delete-dups extended-command-history)= for example to delete duplicates
;; from previous history files.

(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode 1)
  :config
  (setq auto-save-interval 100)
  (setq history-delete-duplicates t)
  (setq history-length 1000)
  (setq savehist-additional-variables '(compile-command kill-ring regexp-search-ring)))


;;** Recursive Editing

;; When in the minibuffer allow using commands that uses the minibuffer
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1) ;; display the recursion level in the minibuffer


;;** Completion framework (corfu.el)

;;*** corfu

(use-package corfu
  :config
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  (setq corfu-auto-prefix 2)
  (setq corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (setq corfu-min-width 100)
  (setq corfu-max-width corfu-min-width)     ; Always have the same width
  (setq corfu-count 15)
  (setq corfu-scroll-margin 4)
  (setq corfu-echo-documentation nil)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  :bind
  ;; Configure SPC for separator insertion
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("M-m" . corfu-move-to-minibuffer))
  :init
  (global-corfu-mode))


;;*** kind-icon

(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;;*** corfu-doc

(use-package corfu-doc
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :after corfu
  :general (:keymaps 'corfu-map
                     ;; This is a manual toggle for the documentation window.
                     [remap corfu-show-documentation] #'corfu-doc-toggle ; Remap the default doc command
                     ;; Scroll in the documentation window
                     "M-n" #'corfu-doc-scroll-up
                     "M-p" #'corfu-doc-scroll-down)
  :config
  (setq corfu-doc-delay 0.5)
  (setq corfu-doc-max-width 70)
  (setq corfu-doc-max-height 80)

  ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
  ;; extra-safe that this is set when corfu-doc is loaded. I do not want
  ;; documentation shown in both the echo area and in the `corfu-doc' popup.
  (setq corfu-echo-documentation nil))


;;***  Dabbrev

(use-package dabbrev
  :config
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)

  (defun diego/dabbrev-full-completion ()
    (interactive)
    (let ((current-prefix-arg 16)) ; 16 C-u C-u = all buffers
      (call-interactively #'dabbrev-completion)))

  :bind (("M-/" . diego/dabbrev-full-completion)
         ("C-M-/" . dabbrev-completion)))


;;** Selection and Narrowing

;; Individual packages that work well together.
;; Vertico, Consult, Embark, Marginalia, and Orderless.

;; I am loving this new combination of tools. Lightweight and fast.

;; All of the above try to use the minibuffer's existing hooks and extension
;; mechanisms, and benefit from large parts of the rest of Emacs using those
;; mechanisms too. Consequently, they all interop with each other and other parts
;; of the Emacs ecosystem. You can pick which you want.

;;*** vertico.el

;; Provides the vertical completion user interface.

(use-package vertico
  :after orderless ; https://github.com/oantolin/orderless/issues/64#issuecomment-868989378
  :straight (:files (:defaults "extensions/*") :includes (vertico-repeat vertico-quick))
  :init
  (add-hook 'vertico-mode-hook (lambda ()
                                 (setq completion-in-region-function
                                       (if vertico-mode
                                           #'consult-completion-in-region
                                         #'completion--in-region))))
  (vertico-mode)
  :config
  ;; (set-face-attribute 'vertico-current nil :background (modus-themes-color 'cyan-intense-bg))
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  (setq vertico-count 20)

  ;; vertico repeat
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (advice-add #'ffap-menu-ask :around (lambda (&rest args)
                                        (cl-letf (((symbol-function #'minibuffer-completion-help)
                                                   #'ignore))
                                          (apply args))))
  :bind (:map
         vertico-map
         ("\C-tab" . #'vertico-quick-insert)
         ("\C-q"     . #'vertico-quick-exit)))


(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  (setq resize-mini-windows 'grow-only)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode))


;;*** consult.el

;; Provides a suite of useful commands using completing-read.

;; https://github.com/minad/consult#use-package-example
;; https://github.com/minad/consult/wiki

;; M-m quick select
;; M-i quick insert
;; M-w copy

(use-package consult
  :after vertico
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-line consult-ripgrep consult-git-grep consult-grep
   consult-bookmark
   ;; consult-recent-file
   consult-xref
   consult--source-project-recent-file
   ;; consult--source-recent-file
   consult--source-project-recent-file consult--source-bookmark
   :preview-key '(:debounce 0.2 any))
  ;; :preview-key (list (kbd "M-SPC") (kbd "C-M-j") (kbd "C-M-k")))


  (setq consult-narrow-key ">")
  (setq consult-widen-key "<")

  ;; TODO 2021-09-14T22:38:11Z
  ;; consult-line-start-from-top

  ;; disable fd for now until https://github.com/minad/consult/wiki#find-files-using-fd
  ;; (setq consult-find-args "fd --color=never --full-path ARG OPTS")
  ;; add --hidden
  (setq consult-ripgrep-args "rg --hidden --glob=!.git/ --glob=!TAGS --null --line-buffered --color=never --max-columns=1000 --path-separator=/ --smart-case --no-heading --line-number .")

  (setq consult-fontify-preserve t)
  (setq consult-preview-key nil)
  ;; (setq consult-project-root-function #'vc-root-dir)
  (setq consult-project-root-function #'diego/current-project-name)

  (defun diego/search-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  (defun diego--recentf-mode-consult ()
    (recentf-mode 1))
  (advice-add 'diego--recentf-mode-consult :before #'consult-recent-file)

  ;; Makes only the open buffers list visible when calling consult-buffer command
  ;; by hiding the other sources, but still allowing the narrowing to recent
  ;; files (by typing f SPC), bookmarks (m SPC) and project buffer and/or files
  ;; (p SPC).  Hide all sources, except normal buffers in consult-buffer by
  ;; default
  (dolist (src consult-buffer-sources)
    (unless (eq src 'consult--source-buffer)
      (set src (plist-put (symbol-value src) :hidden t))))

              ;;;###autoload
  (defun buffer-list-for-mode (mode)
    (seq-filter (lambda (buffer)
                  (eq mode (buffer-local-value 'major-mode buffer)))
                (buffer-list)))

  (defvar kubel-buffer-source
    `(:name     "Kubel"
                :narrow   ?k
                :category buffer
                :state    ,#'consult--buffer-state
                :items  ,(lambda () (mapcar #'buffer-name (buffer-list-for-mode 'kubel-mode)))))
  (add-to-list 'consult-buffer-sources 'kubel-buffer-source 'append)

  (defvar vterm-buffer-source
    `(:name     "Vterm"
                :narrow   ?v
                :category buffer
                :state    ,#'consult--buffer-state
                :items  ,(lambda () (mapcar #'buffer-name (buffer-list-for-mode 'vterm-mode)))))
  (add-to-list 'consult-buffer-sources 'vterm-buffer-source 'append)

              ;;;###autoload
  (defun diego/consult-buffer-for-project ()
    (interactive)
    (require 'consult)
    ;; start with initial narrowing of `p`: project
    (setq unread-command-events (append unread-command-events (list ?p 32)))
    (consult-buffer))

  (defun my/consult-line-forward ()
    "Search for a matching line forward."
    (interactive)
    (consult-line))

  :bind (
         ([remap apropos]                       . #'consult-apropos)
         ([remap bookmark-jump]                 . #'consult-bookmark)
         ([remap evil-show-marks]               . #'consult-mark)
         ([remap goto-line]                     . #'consult-goto-line)
         ([remap imenu]                         . #'consult-imenu)
         ([remap load-theme]                    . #'consult-theme)
         ([remap locate]                        . #'consult-locate)
         ([remap org-goto]                      . #'consult-org-heading)
         ([remap switch-to-buffer]              . #'consult-buffer)
         ([remap switch-to-buffer-other-window] . #'consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . #'consult-buffer-other-frame)
         ([remap yank-pop]                      . #'consult-yank-pop)
         ([remap recentf-open-files]            . #'consult-recent-file)
         ("C-s" . #'my/consult-line-forward)
         :map minibuffer-local-map
         ("C-r" . consult-history)))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package consult-yasnippet
  :after consult)

(use-package consult-dir
  :after (vertico consult)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; https://github.com/gagbo/consult-lsp
(use-package consult-lsp
  :after (consult lsp-mode)
  :bind
  (:map
   lsp-mode-map
   ([remap xref-find-apropos] . #'consult-lsp-symbols)))


;;*** embark.el

;; Embark is a minor mode to allow each minibuffer entry to have multiple actions.

;; https://github.com/oantolin/embark
;; https://github.com/oantolin/embark/wiki/Default-Actions

(use-package embark
  :after wgrep
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config

  ;; If you want to see the actions and their key bindings, but want to use the
  ;; key bindings rather than completing the command name
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (defun diego/embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.
    Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
    (interactive)
    (pcase-let ((`(,type . ,candidates)
                 (run-hook-with-args-until-success 'embark-candidate-collectors)))
      (pcase type
        ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
                         (embark-export)))
        ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
                 (embark-export)))
        ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
                             (embark-export)))
        (x (user-error "embark category %S doesn't support writable export" x)))))

  (defun +embark-collect-hook ()
    (when (eq embark-collect--kind :live)
      (with-selected-window (active-minibuffer-window)
        (setq-local vertico-resize t vertico-count 0)
        (vertico--exhibit))))

  (add-hook 'embark-collect-mode-hook #'+embark-collect-hook)

  :bind
  (("M-a"                     . #'embark-act)
   ("M-d"                     . #'embark-dwim)
   ("C-h B"                   . #'embark-bindings)
   ;; alternative for `describe-bindings'
   ([remap describe-bindings] . #'embark-bindings)
   (:map minibuffer-local-map
         (("C-o" . embark-export)
          (("C-c C-o" . embark-collect-live)
           ("C-c C-e" . diego/embark-export-write)))
         :map embark-collect-mode-map
         (("a" . embark-act)
          ("E" . embark-export)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode-hook . consult-preview-at-point-mode))


;;*** marginalia.el
;; Provides annotations to completion candidates.

(use-package marginalia
  :init
  (marginalia-mode 1)
  :config
  (setq marginalia-truncate-width 120)
  (setq marginalia-field-width 120))


;;*** orderless.el

;; Orderless is a completion-style to allow convenient filters.

(use-package orderless
  :init
  ;; The =basic= completion style is specified as fallback in addition to =orderless= in order to
  ;; ensure that completion commands which rely on dynamic completion tables,
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  ;; Enable `partial-completion' for files to allow path expansion.
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  (setq completion-category-overrides nil)
  (setq completions-format 'one-column)
  (setq completions-detailed t)
  :config

  ;; adapted from https://github.com/minad/consult/wiki#orderless-style-dispatchers-ensure-that-the--regexp-works-with-consult-buffer
  (defvar +orderless-dispatch-alist
    '((?! . orderless-without-literal)
      (?, . orderless-initialism)
      (?= . orderless-literal)))

  ;; Recognizes the following patterns:
  ;; * =literal literal=
  ;; * ,initialism initialism,
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  (setq orderless-component-separator #'orderless-escapable-split-on-space) ;; allow escaping space with backslash
  (setq orderless-style-dispatchers '(+orderless-dispatch)))



;;** Key bindings hints (which-key.el)

;; [[https://github.com/justbur/emacs-which-key][which-key.el]] is a minor mode for Emacs that displays the key bindings following your currently
;; entered incomplete command (a prefix) in a popup.

;; Special SPC, TAB, etc., Single Character a-z,Modifier C-, M-, Other same as default, except single
;; characters are sorted alphabetically

(use-package which-key
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha) ;
  (setq which-key-sort-uppercase-first nil) ; I prefer to have lowercase first when there is for example a k and K
  (setq which-key-max-display-columns nil)
  (setq which-key-min-display-lines 10)
  (setq which-key-side-window-slot -10); A negative value means use a slot preceding (that is, above or on the left of) the middle slot.
  (setq which-key-idle-delay 0.3)
  :config
  (which-key-mode 1))


;;* Search And Replace

;;** Jumping with (avy.el)

;; [[https://github.com/abo-abo/avy][avy]] is used to jump to visible text using chars.

(use-package avy
  :config
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq avy-style 'at-full)
  ;; (define-key evil-motion-state-map (kbd "p") #'avy-goto-word-1)
  ;; (define-key evil-motion-state-map (kbd "P") #'avy-goto-line)
  )


;;** imenu-list.el

;; https://github.com/bmag/imenu-list
;; Emacs plugin to show the current buffer's imenu entries in a seperate buffer.

(use-package imenu-list
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t)
  (set-face-attribute 'imenu-list-entry-face nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-face-0 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-face-1 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-face-2 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-face-3 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-subalist-face-0 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-subalist-face-1 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-subalist-face-2 nil :height 0.7)
  (set-face-attribute 'imenu-list-entry-subalist-face-3 nil :height 0.7))


;;** Writable grep (wgrep.el)

;; With =wgrep= we can directly edit the results of a =grep= and save the
;; changes to all affected buffers.

;; To save all buffers that wgrep has changed, run M-x wgrep-save-all-buffers
;; I then press C-c C-c (wgrep-finish-edit).

;; consult-line -> embark-export to occur-mode buffer -> occur-edit-mode for editing of matches in buffer.
;; consult-grep -> embark-export to grep-mode buffer -> wgrep for editing of all matches.

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))


;;** Visual regex

;; Package `visual-regexp-steroids' allows `visual-regexp' to use regexp engines other than Emacs'; for
;; example, Python or Perl regexps.

(use-package visual-regexp
  :config
  (setq vr/default-replace-preview t))

(use-package visual-regexp-steroids
  :after visual-regexp
  :bind (([remap query-replace-regexp] . #'vr/query-replace)))


;;* Development

;;** Language Server Support (LSP)

;; https://emacs-lsp.github.io/lsp-mode/page/main-features/
;; https://github.com/emacs-lsp/lsp-treemacs

;; - lsp-treemacs-symbols
;; - lsp-treemacs-errors-list
;; - lsp-treemacs-references/lsp-treemacs-implementations
;; - lsp-treemacs-call-hierarchy

;; - consult-lsp-diagnostics
;; - consult-lsp-symbols

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package lsp-mode
  :after (corfu orderless)
  :commands lsp-deferred
  :init
  (setq lsp-keymap-prefix "C-l")
  ;; corfu + orderless
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  :config
  (setq lsp-completion-provider :none) ;; we use Corfu!
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor\\'")

  ;; Project errors on modeline
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-diagnostics-scope :workspace)

  ;; For a UI feedback on headerline of the document
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project))

  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references)

  (evil-add-command-properties #'lsp-find-definition :jump t)
  (evil-add-command-properties #'lsp-goto-type-definition :jump t)
  (advice-add 'lsp-goto-type-definition :before #'evil-set-jump)
  (advice-add 'lsp-find-definition :before #'evil-set-jump)

  ;;test
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05)

  (lsp-enable-which-key-integration t)



  (defun diego--lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  :hook
  ((lsp-completion-mode-hook . my/lsp-mode-setup-completion)
   (lsp-mode-hook . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  ;; Show informations of the symbols on the current line
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions t)

  ;; Add peek feature
  (setq lsp-ui-peek-enable t)
  ;; lsp-ui-peek-show-directory show the directory of files

  ;; Show object documentation at point in a child frame.
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'top)

  ;; imenu
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-imenu-auto-refresh t)

  :hook ((lsp-mode-hook . lsp-ui-mode)))


;;** eglot

(use-package eglot
  :commands eglot eglot-ensure
  :config
  (setq eglot-autoshutdown t))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ([remap xref-find-apropos] . #'consult-eglot-symbols)))



;;** Go

;;*** Get latest gopls

;; sh
;; go install golang.org/x/tools/gopls@latest #


;;*** get latest goimports

;; sh
;; go install golang.org/x/tools/cmd/goimports@latest

;;*** configure local variables for a project

;; Call =add-dir-local-variable=, result:
;; #+begin_example elisp file: .dir-locals.el
;; ;;; Directory Local Variables
;; ;;; For more information see (info "(emacs) Directory Variables")
;; ((go-mode . ((lsp-go-goimports-local . "github.com/Shopify/cloudbuddies")
;;              (gofmt-args . ("-local" "github.com/Shopify/cloudbuddies")))))
;; #+end_example

;;*** go-mode.el

(use-package go-mode
  :config
  ;; (setq-default lsp-go-goimports-local "github.com/Shopify/")
  (setq godef-command "godef") ; original godef
  ;; (setq godef-command "go doc") ; original godef
  (setq gofmt-command "goimports") ; original gofmt
  (setq gofmt-args nil)
  ;; (setq gofmt-args '("-local" "github.com/Shopify/"))

  (defun outline-go-mode-hook ()
    (set (make-local-variable 'outline-regexp) "\\(func \\|\\(.*struct {\\)\\|\\(type \\)\\)"))

  :hook ((go-mode-hook . outline-go-mode-hook)
         (go-mode-hook . lsp-deferred)
         (go-mode-hook . diego--lsp-go-install-save-hooks)))
;; (go-mode-hook .  eglot-ensure)))



;;*** ob-go.el

;; Org-Babel support for evaluating go code.
;; https://github.com/pope/ob-go

(use-package ob-go
  :after (go-mode org)
  :straight (ob-go :type git :host github :repo "pope/ob-go"))


;;*** Custom bindings

(general-nvmap
  :keymaps 'go-mode-map
  :prefix ","
  "a" #'go-tag-add
  "i" #'go-goto-imports
  "." #'godoc-at-point
  "t" '(:ignore t :which-key "test")
  ;; "tt" #'diego/go-run-test-current-function
  "tt" #'go-test-current-test
  "tf" #'go-test-current-file
  "tg" #'go-gen-test-exported)

;;*** flycheck-golangci

(use-package flycheck-golangci-lint
  ;; :after (flycheck go-mode lsp)
  :config
  ;; (setq flycheck-golangci-lint-config "/Users/diegoalvarez/code/go/.golangci.yml")
  (setq flycheck-golangci-lint-fast t)
  (setq flycheck-golangci-lint-tests t)
  (setq flycheck-golangci-lint-enable-linters '(
                                                ;; default
                                                "deadcode" "errcheck" "gosimple" "govet" "ineffassign"
                                                "staticcheck" "structcheck" "typecheck" "unused" "varcheck"
                                                ;; extras
                                                "errname" "errorlint" "exhaustive" "exportloopref" "gocritic" "goconst"
                                                "gocritic" "godot"  "gofmt" "goimports" "gosec" "govet" "ifshort"
                                                "makezero" "nestif" "nilerr" "noctx" "paralleltest" "prealloc" "predeclared"
                                                "revive"  "stylecheck" "testpackage" "unconvert" "unparam"
                                                "varnamelen" "wastedassign" "whitespace" "wsl"
                                                ;; experiment
                                                "wrapcheck"
                                                ;;"goerr113"
                                                ))

  (defun diego--setup-golangci-lint ()
    (flycheck-golangci-lint-setup)
    (push 'golangci-lint flycheck-checkers))
  ;; (flycheck-add-next-checker 'lsp-ui 'golangci-lint))

  :hook ((go-mode-hook . diego--setup-golangci-lint)))


;;*** go-gen-test


(use-package go-gen-test)


;;*** gotest.el

(use-package gotest
  :after go-mode
  :config
  (dolist (elt go-test-compilation-error-regexp-alist-alist)
    (add-to-list 'compilation-error-regexp-alist-alist elt))

  (defun prepend-go-compilation-regexps ()
    (dolist (elt (reverse go-test-compilation-error-regexp-alist))
      (add-to-list 'compilation-error-regexp-alist elt t)))

  (add-hook 'go-mode-hook 'prepend-go-compilation-regexps))


;;*** go-impl

;; go-impl generates method stubs for implementing an interface.


(use-package go-impl)

;;go-impl--completing-function
(defun without-orderless (fn & rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply fn args)))

(advice-add 'command-which-should-not-use-orderless
            :around #'without-orderless)

;;** Rust
;; https://github.com/brotzeit/rustic
;; #+begin_example
;; rustup component add rust-src
;; rustup component add rustfmt
;; cargo install cargo-script # to make it work in org mode babel
;; rustup toolchain install nightly # to use expand
;; cargo install cargo-expand
;; #+end_example


;; https://github.com/brotzeit/rustic#eglot
(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :general
  (general-nvmap
    :keymaps 'rust-mode-map
    :prefix ","
    "b" #'rustic-cargo-build
    "c" '(:ignore t :which-key "cargo")
    "ca" #'rustic-cargo-add
    "cc" #'rustic-cargo-clippy
    "ci" #'rustic-cargo-add-missing-dependencies
    "cu" #'rustic-cargo-upgrade
    "co" #'rustic-cargo-outdated
    "d"  #'rustic-doc-search
    "e" #'rustic-cargo-expand
    "f" #'rustic-cargo-fmt
    "t" '(:ignore t :which-key "test")
    ;; "tt" #'diego/go-run-test-current-function
    "tt" #'rustic-cargo-current-test
    "tf" #'rustic-cargo-test
    "T"  #'lsp-rust-analyzer-related-tests
    "r" #'rustic-cargo-run)

  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  (defun diego--rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))

  :hook ((rustic-mode-hook . lsp-deferred)
         (rustic-mode-hook . diego--lsp-go-install-save-hooks)
         (rustic-mode-hook . diego--rustic-mode-auto-save-hook)))
;;(rust-mode-hook .  eglot-ensure)


;;** Markdown

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :general
  (general-nvmap
    :keymaps 'gfm-mode-map
    :prefix ","
    "p" #'markdown-preview
    "P" #'markdown-live-preview-mode
    "s" #'markdown-insert-gfm-code-block
    "l" #'markdown-insert-link)
  :mode (("\\.md\\'"       . #'gfm-mode)
         ("\\.markdown\\'" . #'gfm-mode)
         ("readme\\.txt\\'" . markdown-mode)
         ("README\\.txt\\'" . markdown-mode))
  :config
  ;; Display remote images
  (setq markdown-display-remote-images t)
  ;; Enable fontification for code blocks
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-header-scaling t)
  (setq markdown-gfm-uppercase-checkbox t)
  (setq markdown-make-gfm-checkboxes-buttons t)

  (setq markdown-italic-underscore t)
  (setq markdown-gfm-additional-languages '("sh" "yaml" "yml"))
  ;; (setq markdown-gfm-additional-languages nil)

  ;; adds highlightjs syntax coloring.
  ;; based on doom config
  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown-dark.min.css"
          "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github-dark.min.css"))

  (setq markdown-xhtml-header-content
        (concat "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
                "<style> body { box-sizing: border-box; max-width: 740px; width: 100%; margin: 40px auto; padding: 0 10px; } </style>"
                "<script id='MathJax-script' async src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js'></script>"
                "<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>"
                ;; follows this to highlight source code blocks https://github.com/highlightjs/highlight.js#using-custom-html
                "<script>document.addEventListener('DOMContentLoaded', () => { document.body.classList.add('markdown-body'); document.querySelectorAll('pre[lang] > code').forEach((code) => { code.classList.add(code.parentElement.lang); }); document.querySelectorAll('pre > code').forEach((code) => { hljs.highlightBlock(code); }); });</script>"))

  (setq markdown-command "pandoc -f gfm --highlight-style pygments --toc -t html --metadata pagetitle=MarkdownPreview"))


;;** Misc


(use-package dockerfile-mode)
(use-package graphql-mode :mode "\\.g\\(?:raph\\)?ql$")
(use-package json-mode)
(use-package nix-mode)
;; (use-package protobuf-mode)
(use-package terraform-mode)
(use-package web-mode
  :mode (("\\.html.erb\\'"       . web-mode)))

;;** yaml

(use-package yaml-mode
  :mode (("\\.yml\\'"       . #'yaml-mode)
         ("\\.yaml\\'"      . #'yaml-mode)
         ("\\.yml.erb\\'"      . #'yaml-mode)
         ("\\.yaml.lock\\'" . #'yaml-mode)))

(use-package yaml
  :straight (:host github :repo "zkry/yaml.el"))

(use-package yaml-pro
  :after yaml
  :straight (:host github :repo "zkry/yaml-pro")
  :general
  (general-nvmap
    :keymaps 'yaml-mode-map
    :prefix ","
    "j" #'yaml-pro-consult-jump
    "ff" #'yaml-pro-fold-at-point
    "fu" #'yaml-pro-unfold-at-point
    ">" #'yaml-pro-indent-subtree
    "<" #'yaml-pro-unindent-subtree
    "K" #'yaml-pro-move-subtree-up
    "J" #'yaml-pro-move-subtree-down
    "'" #'yaml-pro-edit-scalar)
  :hook
  (yaml-mode-hook  . yaml-pro-mode))

;;** Kubernetes

;;*** kubel.el


(use-package kubel
  ;; :straight (kubel :host github :repo "d1egoaz/kubel" :branch "diego/multiple-kubel-buffers")
  :straight (kubel :host github :repo "d1egoaz/kubel" :branch "test")
  :config
  (evil-define-key 'normal 'kubel-yaml-editing-mode "q" #'kill-current-buffer)

  ;; https://github.com/abrochard/kubel/issues/53 https://github.com/abrochard/kubel/pull/44
  ;; (setq kubel-use-namespace-list 'on)
  ;; list namespaces automatically
  (setq kubel-use-namespace-list 'on) ; I'm now using my own branch
  (setq-default kubel-namespace "cloudbuddies")

  (defun diego--kubel-hook ()
    (require 'f)
    (f-mkdir "/tmp/kubel") ; unset default directory
    (f-touch "/tmp/kubel/.project")
    (add-to-list 'savehist-additional-variables 'kubel--context-list-cached)
    (add-to-list 'savehist-additional-variables 'kubel--namespace-list-cached)
    (add-to-list 'savehist-additional-variables 'kubel--kubernetes-resources-list-cached))
  :hook ((kubel-mode-hook . diego--kubel-hook)))

(use-package kubel-evil
  ;; :load-path "/Users/diegoalvarez/code/oss/kubel2"
  :load-path "/Users/diegoalvarez/.emacs.d/straight/repos/kubel/kubel-evil.el"
  :after (kubel evil))


;;** hideshow.el

;; Hideshow mode is a buffer-local minor mode that allows you to selectively
;; display portions of a program, which are referred to as blocks.

;;   hs-hide-block                      C-c @ C-h
;;   hs-show-block                      C-c @ C-s
;;   hs-hide-all                        C-c @ C-M-h
;;   hs-show-all                        C-c @ C-M-s
;;   hs-hide-level                      C-c @ C-l
;;   hs-toggle-hiding                   C-c @ C-c
;;   hs-toggle-hiding                   [(shift mouse-2)]
;;   hs-hide-initial-comment-block


(use-package hideshow
  :config
  (setq hs-hide-comments-when-hiding-all nil) ; dont' hide the comments too when you do a 'hs-hide-all'

  ;; Global hide/show toggle
  (defvar diego--my-hs-hide nil "Current state of hideshow for toggling all.")
  (defun diego/toggle-hideshow-all ()
    "Toggle hideshow all."
    (interactive)
    (setq diego--my-hs-hide (not diego--my-hs-hide))
    (if diego--my-hs-hide
        (hs-hide-all)
      (hs-show-all)))

  (add-to-list 'hs-special-modes-alist
               `(ruby-mode
                 ,(rx (or "def" "class" "module" "{" "[")) ; Block start
                 ,(rx (or "}" "]" "end"))                  ; Block end
                 ,(rx (or "#" "=begin"))                   ; Comment start
                 ruby-forward-sexp nil))
  :hook
  (go-mode-hook . hs-minor-mode))


;;** Compilation mode

;;*** Basic configuration


(use-package compile
  :straight (:type built-in)
  :config
  (setq comint-buffer-maximum-size 8192); Increase comint buffer size.
  (setq comint-input-ignoredups t)
  (setq comint-scroll-to-bottom-on-input t) ; always insert at the bottom
  (setq comint-scroll-to-bottom-on-output nil) ; always add output at the bottom

  (setq compilation-scroll-output 'first-error)
  ;; (setq compilation-scroll-output t)
  ;; (setq compilation-auto-jump-to-first-error t)
  (setq compilation-auto-jump-to-first-error 'if-location-known)
  (setq compilation-always-kill t) ; kill old compile processes before starting the new one
  (setq compilation-ask-about-save nil)  ; save all buffers on `compile'


  (defun compilation-add-separator ()
    "Insert separator in read-only buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (insert "\n---------------------------------\n\n")
      (point-max)
      (comint-set-process-mark)))

  ;; from https://www.reddit.com/r/emacs/comments/wwdpju/comment/ilotsc5/?utm_source=share&utm_medium=web2x&context=3
  (defun meain/compilation-colorcode (_buffer string)
    "Change background color of compilation `_BUFFER' to red on failure."
    (unless (string-prefix-p "finished" string) ; Having color for success was distracting
      (face-remap-add-relative 'default 'lin-red)))
  (add-to-list 'compilation-finish-functions 'meain/compilation-colorcode)

  :bind (:map compilation-mode-map
              ("C-c -" . compilation-add-separator)
              ("-" . compilation-add-separator)
              :map comint-mode-map
              ("C-c -" . compilation-add-separator)))





;;** restclient.el


(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :general
  (general-nvmap
    :keymaps 'restclient-mode-map
    :prefix ","
    "e"  #'restclient-http-send-current
    "E"  #'restclient-http-send-current-raw
    "c"  #'restclient-copy-curl-command))


;;* Productivity

;;** keyfreq.el


(use-package keyfreq
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          backward-char
          evil-backward-char
          evil-forward-char
          evil-forward-word-begin
          evil-mouse-drag-region
          evil-next-line
          evil-next-visual-line
          evil-normal-state
          evil-previous-line
          forward-char
          ignore
          lsp-ui-doc--handle-mouse-movement
          mouse-set-point
          mwheel-scroll
          next-line
          previous-line
          vertico-exit
          ))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;** dired

;;*** dired.el


(use-package dired
  :straight (:type built-in)
  :general
  (general-nvmap
    :keymaps 'dired-mode-map
    :prefix ","
    "w"  #'wdired-change-to-wdired-mode)
  :config
  ;; only one dired buffer when opening directories
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-use-ls-dired nil)
  (setq dired-auto-revert-buffer t))


;;*** dired-sidebar.el


(use-package dired-sidebar
  :general
  (general-nvmap :keymaps 'dired-sidebar-mode-map
    "<mouse-2>" #'dired-sidebar-mouse-subtree-cycle-or-find-file)
  :config
  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-should-follow-file t)
  (setq  dired-sidebar-follow-file-at-point-on-toggle-open t)
  (set-face-attribute 'dired-sidebar-face nil :height 0.8))

(setq dired-sidebar-should-follow-file t)
(setq dired-sidebar-recenter-cursor-on-follow-file nil)

;;** ediff.el


(use-package ediff
  :straight (:type built-in)
  :config
  (setq ediff-split-window-function #'split-window-horizontally)
  ;; stop creating a new frame for navigating changes
  (setq ediff-window-setup-function #'ediff-setup-windows-plain))


;;** diff-hl.el


(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  :hook ((prog-mode-hook       . diff-hl-mode)
         (gfm-mode-hook        . diff-hl-mode)
         (org-mode-hook        . diff-hl-mode)))

;;** Syntax checking (flycheck.el)


(use-package flycheck
  :config
  (global-flycheck-mode 1))


;;** Snippets (yasnippet.el)

;;*** yasnippet.el

;; https://github.com/joaotavora/yasnippet

;; YASnippet is a template system for Emacs. It allows you to type an abbreviation and automatically expand it into function templates.


(use-package yasnippet
  :config
  (setq yas-verbosity 2)
  (setq yas-snippet-dirs `(,(expand-file-name "snippets/" user-emacs-directory)))
  (yas-global-mode 1))


;;*** Snippets collection

;; https://github.com/hlissner/doom-snippets


;; needs files * to download the snippets directories
(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "hlissner/doom-snippets" :files ("*.el" "*"))
  :config
  (yas-global-mode 1))


;;** expand-region.el

;; SPC >

;; https://github.com/magnars/expand-region.el

;; Emacs extension to increase selected region by semantic units.
;; er/expand-region

(use-package expand-region)


;;** Parens (elec-pair.el)


(use-package elec-pair
  :straight (:type built-in)
  :config
  (setq electric-pair-pairs '(
                              (?\" . ?\")
                              (?\` . ?\`)
                              (?\( . ?\))
                              (?\{ . ?\})
                              ))
  (setq electric-pair-text-pairs electric-pair-pairs)
  (setq electric-pair-inhibit-predicate (lambda (c) (char-equal c ?\<)))
  (electric-pair-mode 1))


;;** format-all.el

;; Lets you auto-format source code in many languages using the same command for all languages, instead
;; of learning a different Emacs package and formatting command for each language.


(use-package format-all)


;;** vterm.el


(use-package vterm
  :bind (:map
         vterm-mode-map
         ("<f5>" . nil)
         ("<f6>" . nil)
         ("M-h"  . nil)
         ("M-j"  . nil)
         ("M-k"  . nil)
         ("M-l"  . nil)
         ("C-M-h"  . nil)
         ("C-M-j"  . nil)
         ("C-M-k"  . nil)
         ("C-M-l"  . nil)
         )
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 500000)

  (defun diego--vterm-hook ()
    ;; Don't prompt about dying processes when killing vterm
    (setq confirm-kill-processes nil)
    ;; Prevent premature horizontal scrolling
    (setq hscroll-margin 0))

  (load (expand-file-name "diego-vterm.el" user-emacs-directory))
  :hook ((vterm-mode-hook . diego--vterm-hook)))


;;** flyspell.el

;; `z=` to correct word.


(use-package flyspell
  :after org
  :config
  (setq ispell-program-name "aspell")
  :hook ((prog-mode-hook       . flyspell-prog-mode)
         (gfm-mode-hook        . flyspell-prog-mode)
         (text-mode-hook       . flyspell-mode)
         (git-commit-mode-hook . flyspell-mode)
         (org-mode-hook        . flyspell-mode)))

(use-package flyspell-correct
  :after flyspell
  :bind (([remap ispell-word] . #'flyspell-correct-at-point))
  :config
  (setq flyspell-correct-interface #'flyspell-correct-dummy)) ; provides save, skip


;;** Define word

;; Use directly this server instead of trying localhost.
;; dict.org uses Webster 1913 dictionary.

(use-package dictionary
  :straight (:type built-in)
  :config
  (setq dictionary-server "dict.org"))



(use-package define-word)

;;** langtool.el


(use-package langtool
  :config
  (setq langtool-mother-tongue "en")
  (setq langtool-default-language "en-US")
  (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/5.5/libexec/languagetool-commandline.jar"))


;;** buff-move.el

;; Package `buffer-move' provides simple commands to swap Emacs windows: `buf-move-up',
;; `buf-move-down', `buf-move-left', `buf-move-right'.


(use-package buffer-move)


;;** ripgrep.el


(use-package ripgrep)


;;** iedit.el


(use-package iedit
  :config
  (add-to-list 'warning-suppress-types '(iedit)) ; to avoid warn edit default key %S is occupied by %s

   ;;;###autoload
  (defun diego/iedit-scoped (orig-fn)
    "Call `iedit-mode' with function-local scope, or global scope if called with a universal prefix."
    (interactive)
    (pcase-exhaustive current-prefix-arg
      ('nil (funcall orig-fn '(0)))
      ('(4) (funcall orig-fn))))

  (advice-add #'iedit-mode :around #'diego/iedit-scoped))


;;** which-function-mode.el


(which-function-mode 1)
;; Show the current function name in the header line
(setq-default header-line-format
              '((which-function-mode ("" which-func-format " "))))

;; We remove Which Function Mode from the mode line, because it's mostly
;; invisible here anyway.
(setq mode-line-misc-info (assq-delete-all 'which-function-mode mode-line-misc-info))


;;** bookmarks.el


(use-package bookmark
  :after org
  :straight (:type built-in)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations t)
  (setq bookmark-set-fringe-mark t) ; Emacs28
  (setq bookmark-save-flag 1)

  (setq bookmark-default-file "~/Documents/deft/bookmarks.el")
  (setq bookmark-watch-bookmark-file 'silent)

  (defun diego--bookmark-url-handler (bookmark)
    (browse-url (bookmark-prop-get bookmark 'url)))

  (defun diego/bookmark-set-url (url name)
    (interactive "sBookmark URL: \nsBookmark name: ")
    (if (assoc name bookmark-alist)
        (user-error "%s is already bookmarked" name)
      (push `(,name . ((handler . ,#'diego--bookmark-url-handler)(url . ,url)(filename . ,url)))
            bookmark-alist))))

;;** highlight-parentheses.el

(use-package highlight-parentheses
  :after modus-themes
  :config
  (defvar my-highlight-parentheses-use-background t
    "Prefer `highlight-parentheses-background-colors'.")

  (setq my-highlight-parentheses-use-background nil) ; Set to nil to disable backgrounds

  (defun my-modus-themes-highlight-parentheses ()
    (modus-themes-with-colors
      ;; Our preference for setting either background or foreground
      ;; styles, depending on `my-highlight-parentheses-use-background'.
      (if my-highlight-parentheses-use-background

          ;; Here we set color combinations that involve both a background
          ;; and a foreground value.
          (setq highlight-parentheses-background-colors (list cyan-refine-bg
                                                              magenta-refine-bg
                                                              green-refine-bg
                                                              yellow-refine-bg)
                highlight-parentheses-colors (list cyan-refine-fg
                                                   magenta-refine-fg
                                                   green-refine-fg
                                                   yellow-refine-fg))

        ;; And here we pass only foreground colors while disabling any
        ;; backgrounds.
        (setq highlight-parentheses-colors (list green-intense
                                                 magenta-intense
                                                 blue-intense
                                                 red-intense)
              highlight-parentheses-background-colors nil)))

    ;; Our changes must be evaluated before enabling the relevant mode, so
    ;; this comes last.
    (global-highlight-parentheses-mode 1))

  ;; Include this if you also want to make the parentheses bold:
  (set-face-attribute 'highlight-parentheses-highlight nil :inherit 'bold)
  (global-highlight-parentheses-mode 1)

  :hook ((modus-themes-after-load-theme-hook . my-modus-themes-highlight-parentheses)))


;;** highlight-indent-guides.el


(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  :hook ((prog-mode-hook . highlight-indent-guides-mode)))


;;** lin.el


(use-package lin
  :straight (:host github :repo "protesilaos/lin")
  :init
  (setq lin-mode-hooks
        '(
          dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          kubel-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          occur-mode-hook
          tabulated-list-mode-hook))
  :config
  (setq lin-face 'lin-blue-override-fg)
  (lin-global-mode 1))


;;** blamer.el


(use-package blamer
  :straight (:type git :host github :repo "artawower/blamer.el")
  :init
  (setq blamer-idle-time 1.0)
  (setq blamer-min-offset 60))
;; :config
;; (global-blamer-mode 1))


;;** pulsar.el (based on pulse.el)

;; Never lose the cursor again.


(use-package pulsar
  :straight (:host github :repo "protesilaos/pulsar")
  :init
  (setq pulsar-pulse-functions
        '(
          backward-page
          bookmark-jump
          delete-other-windows
          delete-window
          evil-avy-goto-line
          evil-goto-definition
          evil-scroll-down
          evil-scroll-line-to-bottom
          evil-scroll-line-to-center
          evil-scroll-line-to-top
          evil-scroll-up
          evil-window-left
          evil-window-right
          evil-window-split
          forward-page
          kill-current-buffer
          lsp-find-references
          lsp-find-definition
          lsp-find-implementation
          move-to-window-line-top-bottom
          other-window
          recenter-top-bottom
          reposition-window
          scroll-down
          scroll-down-command
          scroll-up
          scroll-up-command
          tab-close
          tab-new
          tab-next
          windmove-down
          windmove-left
          windmove-right
          windmove-swap-states-down
          windmove-swap-states-left
          windmove-swap-states-right
          windmove-swap-states-up
          windmove-up
          xref-find-references
          xref-find-definitions))

  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.2)
  (setq pulsar-iterations 6)
  (setq pulsar-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta)
  (pulsar-global-mode 1)

  (defun diego--pulsar-setup ()
    )
  ;; (pulsar-global-mode 1))

  :hook (
         ;; (consult-after-jump-hook . pulsar-recenter-middle)
         ;; (consult-after-jump-hook . pulsar-reveal-entry)
         ;; (imenu-after-jump-hook . pulsar-recenter-middle)
         ;; (imenu-after-jump-hook . pulsar-reveal-entry)
         (after-init-hook . diego--pulsar-setup)))


;;** string-inflection.el


(use-package string-inflection)

;;** undo-fu.el


(use-package undo-fu
  :config
  (setq undo-limit 8000000           ; 8mb (default is 160kb)
        undo-strong-limit 8000000   ; 8mb   (default is 240kb)
        undo-outer-limit 48000000))  ; 48mb  (default is 24mb)

(use-package undo-fu-session
  :after undo-fu
  :config
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-incompatible-major-modes '(vterm-mode kubel-mode))
  ;; (global-undo-fu-session-mode)
  :hook ((prog-mode-hook       . undo-fu-session-mode)
         (gfm-mode-hook        . undo-fu-session-mode)
         (org-mode-hook        . undo-fu-session-mode)
         (text-mode-hook       . undo-fu-session-mode)))

;;** mixed-pitch.el

(use-package mixed-pitch
  :hook (
         (org-mode-hook       . mixed-pitch-mode)
         (gfm-mode-hook        . mixed-pitch-mode)))
;; (text-mode-hook       . mixed-pitch-mode)))


;;* Org Mode

;;** org.el


(use-package org
  :general
  ;; I prefer C-c C-c over C-c ' (more consistent)
  (:keymaps
   'org-src-mode-map
   "C-c C-c" #'org-edit-src-exit)
  (:keymaps
   'org-mode-map
   "C-j"  #'org-move-subtree-down
   "C-k"  #'org-move-subtree-up
   "M-h" nil
   "M-l" nil)
  ;; local leader
  (general-nvmap
    :keymaps 'org-mode-map
    :prefix ","
    "'" #'org-edit-special
    "e" #'org-export-dispatch
    "h" #'org-toggle-heading
    "i" #'org-toggle-item
    "q" #'org-set-tags-command
    "t" #'org-todo
    "x" #'org-toggle-checkbox
    "a"  '(:ignore t :which-key "attachments")
    "aa" #'org-attach
    "ar" #'org-attach-reveal
    "au" #'org-attach-url
    "ac" #'org-download-screenshot
    "l"  '(:ignore t :which-key "link")
    "li" #'org-id-store-link
    "ll" 'org-insert-link
    "ls" 'org-store-link
    "d"   '(:ignore t :which-key "date/deadline")
    "dd" #'org-deadline
    "ds" #'org-schedule
    "dt" #'org-time-stamp)
  :init
  (setq org-directory "~/Documents/deft")
  (setq org-agenda-files (list "~/Documents/deft/journal.org" "~/Documents/deft/gtd-inbox.org" "~/Documents/deft/gtd-personal.org" "~/Documents/deft/gtd-work.org" ))
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-agenda-deadline-faces
        '((1.001 . error)
          (1.0 . org-warning)
          (0.5 . org-upcoming-deadline)
          (0.0 . org-upcoming-distant-deadline)))
  (setq org-agenda-span 'month); or 'week
  (setq org-attach-id-dir (file-name-as-directory (concat (file-name-as-directory org-directory) "images")))
  (setq org-default-notes-file (concat (file-name-as-directory org-directory) "notes.org"))
  (setq org-refile-targets '(("~/Documents/deft/gtd-inbox.org" :maxlevel . 1) ("~/Documents/deft/gtd-personal.org" :level . 1) ("~/Documents/deft/gtd-work.org" :maxlevel . 2)))
  :config
  (setq org-blank-before-new-entry '((heading . always) (plain-list-item . nil)))
  (setq org-clock-out-remove-zero-time-clocks nil)
  ;; (setq org-cycle-emulate-tab 'white) ; allows to collapse the current outline (call org-cycle)
  (setq org-confirm-babel-evaluate nil)
  (setq org-edit-src-content-indentation 0) ; not need to waste space
  (setq org-adapt-indentation nil) ; emacs sometimes hangs when using evil to open line above/below
  (setq org-ellipsis "⌄ ")
  (setq org-fontify-quote-and-verse-blocks t)
  ;; (setq org-hide-leading-stars nil)
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers nil)
  (setq org-insert-heading-respect-content nil) ; Insert Org headings at point, not after the current subtree
  (setq org-log-into-drawer t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-window-setup 'reorganize-frame)
  (setq org-startup-with-inline-images t)
  (setq org-todo-keywords '((sequence "TODO(t!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c!)" "IN-PROGRESS(i!)")))
  (setq org-capture-templates
        '(
          ;; example:
          ;;   "t"                               = key
          ;;   "Todo"                            = description
          ;;   entry                             = type
          ;;   (file+headline "file" "tasks")    = target
          ;;   ""                                = template
          ;;   :prepend t                        = properties
          ;; https://orgmode.org/manual/Template-expansion.html
          ("t" "Todo" entry (file+headline "~/Documents/deft/gtd-inbox.org" "Inbox")
           "* TODO %?\nCreated on on %U\n" :prepend t :empty-lines 1)
          ("l" "Link" entry (file+headline "~/Documents/deft/notes.org" "Links")
           "* %? %^L %^g \n%T" :prepend t)
          ("n" "Note" entry (file+headline "~/Documents/deft/notes.org" "Notes")
           "* %^{title}%^g\n%T\n\n%?" :prepend t)
          ("j" "Journal" entry (file+olp+datetree "~/Documents/deft/journal.org")
           "* %?\nSCHEDULED: <%(org-read-date nil nil \"today\")>" :clock-in t :clock-resume t))))


;;** org-download.el


(use-package org-download
  :after org
  :commands org-download-screenshot
  :config
  (setq org-download-heading-lvl nil)
  (setq org-download-image-dir org-attach-directory)
  (setq org-download-image-html-width 500)
  (setq org-download-method 'attach)
  (setq org-download-screenshot-method "screencapture -i %s")
  (setq org-download-timestamp "_%Y%m%d_%H%M%S"))


;;** evil-org.el

;; https://github.com/hlissner/evil-org-mode

;; key	explanation
;; gh, gj, gk, gl	navigate between elements
;; vae	select an element

;; |------+----------------------+-------------------|
;; | key  | function             | explanation       |
;; |------+----------------------+-------------------|
;; | =gh= | org-element-up       | parent of element |
;; | =gj= | org-forward-element  | next element      |
;; | =gk= | org-backward-element | previous element  |
;; | =gl= | org-down-element     | first subelement  |
;; | =gH= | evil-org-top         | top-level heading |
;; |------+----------------------+-------------------|

;; all keybindings https://raw.githubusercontent.com/hlissner/evil-org-mode/master/doc/keythemes.org

(use-package evil-org
  :after (evil org)
  :straight (:host github :repo "hlissner/evil-org-mode")
  :config
  ;; enable bindings, remove `additional` as I want to use M-hjkl for different things
  (setq evil-org-key-theme '(heading insert navigation textobjects))
  (evil-org-set-key-theme)

  (general-nmap :keymaps 'evil-org-mode-map
    "M-h" nil
    "M-j" nil
    "M-k" nil
    "M-l" nil)

  (defun diego--org-set-key-theme ()
    (evil-org-set-key-theme))
  :hook ((org-mode-hook . evil-org-mode)
         (evil-org-mode-hook . diego--org-set-key-theme)))



;;** Make org-capture start in insert mode


(add-hook 'org-capture-mode-hook #'evil-insert-state)


;;** Org-Babel


(org-babel-do-load-languages 'org-babel-load-languages
                             '(
                               (dot . t)
                               (shell . t)
                               (gnuplot . t)
                               (latex . t)
                               ))


;;** mermaid support


(use-package mermaid-mode)

(use-package ob-mermaid
  :after mermaid-mode)


;;** Create table of contents

;; To use, add a =:TOC:= tag to the headline.
;; Every time the file is saved, it'll be auto-updated with the current table of contents.

;; The table of contents heading may also be set with these tags:

;; - =:TOC_#:= Sets the maximum depth of the headlines in the table of
;;   contents to the number given, e.g. :TOC_3: for
;;   3 (default for plain :TOC: tag is 2).

;; - =:TOC_#_gh:= Sets the maximum depth as above and also uses
;;   GitHub-style anchors in the table of contents (the
;;   default).  The other supported style is :TOC_#_org:,


(use-package toc-org
  :after (org markdown-mode)
  :config
  (setq toc-org-max-depth 2)
  :hook ((org-mode-hook . toc-org-mode)
         (markdown-mode-hook . toc-org-mode)))


;;** iMenu org depth

;; Increase the maximum level for Imenu access to Org headlines.


(setq org-imenu-depth 6)


;;* Misc

;;** helpful.el

;; [[https://github.com/Wilfred/helpful][helpful.el]] is an alternative to the built-in Emacs help that provides much more contextual information.


(use-package helpful
  :bind (
         ([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-symbol]   . #'helpful-symbol)
         ([remap describe-key]      . #'helpful-key))
  :general
  (general-nmap :keymaps 'helpful-mode-map
    "q" #'kill-buffer-and-window))

(setq help-window-select t)


;;** all-the-icons.el

;; To have some icons available in doom mode line.


(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.9))

(use-package all-the-icons-dired
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode 1))


;;** World Clock

;; Tz zones: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones.
;; =format-time-string= for time format. ISO 8601 format =%FT%T%z=.


(use-package time
  :general
  (general-nmap :keymaps 'world-clock-mode-map
    "q" #'kill-buffer-and-window)
  :config
  (setq zoneinfo-style-world-list '(("etc/UTC" "UTC")
                                    ("America/Vancouver" "PT")
                                    ("America/New_York" "ET")
                                    ("America/Bogota" "Bogota")
                                    ("America/Toronto" "Toronto")))
  (setq world-clock-time-format "%A %d %B %R (%Z %z) %FT%T%z")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  :hook
  (after-init-hook . display-time-mode))


;;** olivetti.el


(use-package olivetti
  :config
  (setq olivetti-minimum-body-width 200)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode diego/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters."
    :init-value nil
    :global nil
    (if diego/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil))))


;;** RSS (elfeed.el)

;; The best RSS reader.


(use-package elfeed
  :after olivetti
  :general
  (general-nmap :keymaps 'elfeed-search-mode-map
    "c" #'elfeed-search-clear-filter
    "s" #'elfeed-search-live-filter
    "r" #'elfeed-search-untag-all-unread
    "," #'diego/elfeed-filter)
  (general-nmap :keymaps 'elfeed-show-mode-map
    "C-n" #'elfeed-show-next
    "C-p" #'elfeed-show-prev)
  :commands elfeed
  :config
  (setq elfeed-search-date-format '("%a %b-%d" 10 :left))
  (setq elfeed-search-filter "@2-week-ago +unread")
  (setq elfeed-search-title-max-width 120)
  (setq elfeed-show-truncate-long-urls t)
  (setq elfeed-sort-order 'ascending)

  (defun diego/elfeed-filter-do ()
    (interactive)
    (let ((tags (mapconcat 'identity (transient-args 'diego/elfeed-filter) " ")))
      (elfeed-search-clear-filter)
      (elfeed-search-set-filter (format "@2-weeks-ago +unread %s" tags))
      (goto-char (point-min))))

  (transient-define-prefix diego/elfeed-filter ()
    [["Arguments"
      ("a" "apple" "+apple")
      ("c" "Tech Crunch" "+techcrunch")
      ("e" "emacs" "+emacs")
      ("h" "Hacker News" "+hnews")
      ("l" "linux" "+linux")
      ("t" "top" "+top")
      ("s" "sre" "+sre")
      ("v" "verge" "+theverge")]
     ["Reddit"
      ("p" "r/Programming" "+programming")]
     ["Actions"
      ("f" "apply" diego/elfeed-filter-do)
      ("u" "update" elfeed-update)]])

  :hook ((elfeed-search-mode-hook . diego/olivetti-mode)
         (elfeed-show-mode-hook . diego/olivetti-mode)))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list "~/Documents/deft/elfeed.org"))
  (elfeed-org))


;;** rainbow-mode.el


(use-package rainbow-mode)


;;** emacs-everywhere.el

;; By default, emacs-everywhere-insert-selection is a hook in
;; emacs-everywhere-init-hooks, and will insert the last text selection into your
;; new buffer. To clear this, type DEL or C-SPC before anything else.

;; Once you’ve finished and want to insert the text into the window you triggered
;; Emacs Everywhere from, just press C-c C-c.


(use-package emacs-everywhere
  :after ox-gfm
  :init
  (setq emacs-everywhere-markdown-apps '("Slack"))
  (setq emacs-everywhere-markdown-windows
        '("Stack Exchange" "Stack Overflow" "Reddit" ; Sites
          "Pull Request" "Issue" "Comparing .*\\.\\.\\." ; Github
          "Discord" "Slack"))

  ;; use by default gfm-mode instead of using org-mode and pandoc
  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-major-mode-org-or-markdown)
  (remove-hook 'emacs-everywhere-init-hooks #'org-mode)
  (add-hook 'emacs-everywhere-init-hooks #'gfm-mode)

  (setq emacs-everywhere-frame-parameters
        `((name . "emacs-everywhere")
          (width . 120)
          (height . 20))))

;; to signal emacs-everywhere to use org-gfm-export-to-markdown
;; currently not used as I'm always using gfm-mode
(use-package ox-gfm
  :after org
  :config
  (require 'ox-gfm nil t))

;;** erc


(use-package erc
  :straight (:type built-in)
  :config
  (setq erc-auto-query 'bury)
  (setq erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs")))
  (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (setq erc-kill-buffer-on-part nil)
  (setq erc-kill-server-buffer-on-quit t)
  (setq erc-nick "d1egoaz")
  (setq erc-prompt-for-password nil)
  (setq erc-rename-buffers t) ; Rename server buffers to reflect the current network name instead of SERVER:PORT (e.g., "freenode" instead of "irc.freenode.net:6667").
  (setq erc-server "irc.libera.chat"))


;;* Keybindings

;;** Simplify Leader Bindings (general.el)

;; [[https://github.com/noctuid/general.el][general.el]] allows us to set keybindings.

;; =general.el= is installed in early-init.el.*

;;** Bindings


;; ** Global Keybindings

;; repeat last macro with Q, use macro and then Q to repeat it
(define-key evil-normal-state-map "Q" (kbd "@@"))

;; unset "C-k" in evil insert, fixes binding for corfu
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)

(evil-define-key 'visual global-map (kbd ">") 'my/evil-shift-right)
(evil-define-key 'visual global-map (kbd "<") 'my/evil-shift-left)

;; ESC Cancels All
(define-key global-map [escape] #'keyboard-escape-quit)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-M-o") #'ace-window)

(global-set-key (kbd "s-b") #'consult-buffer)
(global-set-key (kbd "s-r") #'consult-recent-file)
(global-set-key (kbd "s-w") #'delete-window)
(global-set-key (kbd "M-c") #'evil-commentary-line)

(global-set-key (kbd "C-M-h") #'previous-buffer)
(global-set-key (kbd "C-M-l") #'next-buffer)

;; Https://emacs.stackexchange.com/questions/22266/backspace-without-adding-to-kill-ring
(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
   With argument, do this that many times.
   This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
   With argument, do this that many times.
   This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))
(global-set-key (kbd "<M-backspace>") 'my-backward-delete-word)

;; (general-nvmap
;; "C-o" #'backward-global-mark
;; "C-i" #'forward-global-mark)

(general-nvmap
  "g-r" #'xref-find-references
  "g-i" #'lsp-find-implementation)
;;::// TODO: @d1egoaz 2022-04-05: move this
(setq xref-search-program 'ripgrep)

(general-define-key
 :states '(normal visual emacs motion) ; some modes for some reason start in motion mode
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "C-M-x"
 "" nil ; to fix evil in some buffers, like *Messages*
 "SPC"     #'(execute-extended-command :which-key "M-x")
 "'"       #'(vertico-repeat-last :which-key "Resume last search")
 "\""       #'(vertico-repeat-select :which-key "Resume last session")
 ">"       #'(er/expand-region :which-key "Expand region")
 "u"       #'(universal-argument :which-key "Universal argument")
 "U"       #'(universal-argument-more :which-key "Universal argument more")
 "x"       #'((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "Scratch buffer")
 ;; Tabs
 "TAB"     '(:ignore t :which-key "workspaces")
 "TAB s" #'(tab-switch :which-key "Switch workspace")
 "TAB k"   #'(tab-bar-close-tab :which-key "Close workspace")
 "TAB K"   #'(tab-bar-close-tab-by-name :which-key "Close workspace by name")
 "TAB h"   #'(tab-previous :which-key "Previous workspace")
 "TAB l"   #'(tab-next :which-key "Next workspace")
 "TAB TAB"   #'(tab-recent :which-key "Recent workspace")
 "TAB t"   #'(diego/toggle-workspaces-enabled :which-key "Toggle tabs as workspaces")
 ;; Apps
 "a"       '(:ignore t :which-key "apps")
 "aa"   #'((lambda () (interactive) (org-agenda-list)) :which-key "Org Agenda Week")
 "aA"      #'org-agenda
 "ac"      #'(world-clock :which-key "World clock")
 "aC"      #'calendar
 "ad"      #'(dired-jump :which-key "Dired current dir")
 "ae"      #'(elfeed :which-key "Elfeed - RSS")
 "ak"      #'(kubel :which-key "Kubel")
 "am"      #'(diego/project-minimap :which-key "Project minimap")
 "ap"      #'(list-processes :which-key "List process")
 "aP"      #'(list-processes :which-key "Kill process")
 "at"      #'dired-sidebar-show-sidebar
 "aT"      #'dired-sidebar-toggle-sidebar
 "av"       #'((lambda () (interactive) (diego/vterm "*vterm*")) :which-key "Vterm in side window")
 "aV"      #'(diego/vterm :which-key "Vterm in full window")
 ;; Buffers
 "b"       '(:ignore t :which-key "buffers")
 ;; "bb"      #'(consult-buffer :which-key "Switch buffer")
 "ba"      #'(consult-buffer :which-key "Switch buffer")
 "bb"      #'(consult-project-buffer :which-key "Project buffer")
 "bB"      #'(ibuffer-list-buffers :which-key "Ibuffer list buffers")
 "bc"      #'(clone-indirect-buffer-other-window :which-key "Clone indirect buffer other window")
 "bC"      #'((lambda () (interactive) (switch-to-buffer "*compilation*")) :which-key "Compilation buffer")
 "bd"      #'((lambda () (interactive) (diff-buffer-with-file (current-buffer))) :which-key "diff-buffer-with-file")
 "bg"      #'vc-diff
 "bw"      #'(kill-buffer-and-window :which-key "Kill current buffer and window")
 "be"      #'(diego/safe-erase-buffer :which-key "Erase buffer")
 "bE"      #'(view-echo-area-messages :which-key "Echo area buffer")
 "bf"      #'(format-all-buffer :which-key "Format buffer")
 "bi"      #'(diego/indent-buffer :which-key "Indent buffer")
 "bk"      #'(kill-buffer :which-key "Kill selected buffer")
 "bl"      #'(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
 "br"      #'revert-buffer-quick

 "bm"      '(:ignore t :which-key "move buffer")
 "bmk"     #'(buf-move-up :which-key "Move up")
 "bmj"     #'(buf-move-down :which-key "Move down")
 "bmh"     #'(buf-move-left :which-key "Move left")
 "bml"     #'(buf-move-right :which-key "Move right")

 "bM"      #'view-echo-area-messages
 "bn"      #'(next-buffer :which-key "Next buffer")
 "bp"      #'(previous-buffer :which-key "Previous buffer")
 "bs"      #'(basic-save-buffer :which-key "Save buffer")
 "bS"      #'(evil-write-all :which-key "Save all buffers")
 "by"      #'(diego/copy-buffer-name :which-key "Yank buffer name")
 "bY"      #'(diego/copy-whole-buffer-to-clipboard :which-key "Copy buffer to clipboard")
 ;; Compile
 "c"       '(:ignore t :which-key "compile")
 "cc"      #'diego/project-compile
 "cC"      #'diego/recompile
 "cr"      #'diego/recompile
 "cd"     #'diego/dev
 "cD"     #'diego/dev-project
 ;; Evaluate elisp expressions
 "e"       '(:ignore t :which-key "eval/error")
 "eb"      #'(eval-buffer :which-key "Eval elisp in buffer")
 "ed"      #'(eval-defun :which-key "Eval defun")
 "ee"      #'(eval-expression  :which-key "Eval elisp expression")
 "el"      #'(eval-last-sexp :which-key "Eval last sexression")
 "er"      #'(eval-region :which-key "Eval region")
 "en"      #'(next-error :which-key "Next error")
 "ep"      #'(previous-error :which-key "Previous error")
 ;; Files
 "f"       '(:ignore t :which-key "files")
 "fC"      #'(copy-file :which-key "Copy file")
 "fD"      #'(diego/delete-file :which-key "Delete file")
 "ff"      #'(find-file :which-key "Find file")
 "fj"      #'((lambda () (interactive) (find-file "~/Documents/deft/journal.org")) :which-key "Journal file")
 "fl"      #'((lambda () (interactive) (dired-sidebar-follow-file)) :which-key "Follow file in sidebar")
 "fp"      #'((lambda () (interactive) (find-file (expand-file-name "config.el" user-emacs-directory))) :which-key "Edit private config")
 "fr"      #'(consult-recent-file :which-key "Recent files")
 "fR"      #'(rename-file :which-key "Rename file")
 "fs"      #'(save-buffer :which-key "Save file")
 "fS"      #'(write-file :which-key "Save file as...")
 "fy"      #'(diego/copy-file-name :which-key "Yank file path")
 ;; Git
 "g"       '(:ignore t :which-key "git")
 "ga"      #'(vc-annotate :which-key "Annotate, show edit history") ;; f vc-annotate-find-revision-at-line, l vc-annotate-show-log-revision-at-line, D, F, J
 "gb"      #'magit-log-buffer-file
 "gs"      #'(magit-status :which-key "Magit status")
 "gF"      #'magit-fetch-all
 "gg"      #'magit-dispatch

 "gl"      '(:ignore t :which-key "git link")
 "glh"      #'git-link-homepage
 "gll"     #'(git-link :which-key "git link in current branch")
 "glm"     #'((lambda () (interactive) (let ((git-link-default-branch "master"))(call-interactively #'git-link))) :which-key "git link in master")

 "gL"      #'magit-log
 "gr"      #'vc-region-history ;; same as a 'magit-log-buffer-file if a region is sele
 "gv"      #'((lambda () (interactive)(vc-revision-other-window "master")) :which-key "Visit file in master branch")
 ;; Help/Highlight
 "h"       '(:ignore t :which-key "help/highlight")
 "ha"      #'(consult-apropos :which-key "Describe apropos")
 "hc"      #'describe-command
 "hC"      #'describe-char
 "hf"      #'describe-function
 "hF"      #'describe-face
 "hk"      #'(helpful-key :which-key "Describe key")
 "hK"      #'(describe-keymap :which-key "Describe keymap")
 "hi"      #'info-display-manual
 "hl"      #'(find-library :which-key "Describe library")
 "hm"      #'describe-mode
 "hp"      #'(helpful-at-point :which-key "Describe at point")
 "ho"      #'describe-symbol
 "hv"      #'describe-variable
 "hr"      #'((lambda () (interactive) (load-file (expand-file-name "init.el" user-emacs-directory))) :which-key "Reload emacs config")
 "H"       '(:ignore t :which-key "highlight")
 "Ho"      #'highlight-symbol-at-point
 "Hl"      #'highlight-lines-matching-regexp
 "Hr"      #'highlight-regexp
 "Hu"      #'unhighlight-regexp
 "hw"       '(:ignore t :which-key "help with words")
 "hwd"      #'define-word-at-point
 "hws"      #'dictionary-search
 "hwl"      #'langtool-check
 ;; Help Profiler
 "hP"       '(:ignore t :which-key "profiler")
 "hPs"      #'(profiler-start :which-key "Profiler start")
 "hPk"      #'(profiler-stop :which-key "Profiler stop")
 "hPr"      #'(profiler-report :which-key "Profiler report")
 ;; Insert
 "i"       '(:ignore t :which-key "insert")
 "iu"      #'(insert-char :which-key "Unicode char")
 "is"      #'(consult-yasnippet :which-key "Snippet")
 "ik"      #'(diego/evil-insert-line-above :which-key "Line above")
 "ij"      #'(diego/evil-insert-line-below :which-key "Line below")
 "iy"      #'(consult-yank-pop :which-key "Insert Yank")
 ;; Jump
 "j"       '(:ignore t :which-key "jump")
 "je"      #'consult-compile-error
 "ji"      #'imenu ; consult-imenu
 "jI"      #'consult-imenu-multi
 "jL"      #'imenu-list
 "jb"      #'(bookmark-jump :which-key "Jump to bookmark")
 "jB"      #'(bookmark-set :which-key "Set bookmark")
 "jh"      #'consult-history
 "jj"      #'(avy-goto-char-timer :which-key "Jump to char")
 "jl"      #'(avy-goto-line :which-key "Jump to line")
 "jh"      #'(evil-show-jumps :which-key "Jump history")
 "jm"      #'(evil-show-marks :which-key "Jump to mark")
 "jM"      #'consult-kmacro
 "jo"      #'consult-outline
 "ju"      #'((lambda () (interactive) (browse-url (thing-at-point 'url t))) :which-key "Browse URL at point")
 "jU"      #'(ffap-menu :which-key "Jump to URL")
 "jr"      #'(consult-buffer :which-key "Jump to recent file/buffer")
 "jp"      #'(diego/open-url :which-key "Jump to URL by search provider")
 "js"      #'(avy-goto-symbol-1 :which-key "Jump to begginning of word")
 ;; Kill
 "kk"      #'(kill-current-buffer :which-key "Kill current buffer")
 "kh"      #'((lambda () (interactive) (kill-matching-buffers "\\*helpful" nil t)) :which-key "Kill help buffers")
 "kv"      #'((lambda () (interactive) (kill-matching-buffers "\\*vterm" nil t)) :which-key "Kill vterm buffers")
 "k TAB"    #'(tab-bar-close-tab :which-key "Kill workspace tab")
 "kw"      #'(kill-buffer-and-window :which-key "Kill current buffer and window")
 ;; LSP
 "l"       '(:ignore t :which-key "lsp")
 "la"      #'xref-find-apropos
 "ld"      #'xref-find-definitions
 ;; "lli"      #'eglot-find-implementation
 "li"      #'lsp-find-implementation
 "lr"      #'xref-find-references
 ;; "llt"      #'eglot-find-typeDefinition
 "lt"      #'lsp-goto-type-definition
 ;; "lc"      #'lsp-treemacs-call-hierarchy
 "le"      #'flycheck-list-errors
 "lE"      #'lsp-rust-analyzer-expand-macro
 "lk"      #'lsp-describe-thing-at-point
 ;; "lr"      #'eglot-rename
 "lR"      #'lsp-rename
 ;; "lx"      #'eglot-code-actions
 "lx"      #'lsp-execute-code-action
 "ls"       #'(lsp-signature-activate :which-key "show signature")
 "lh"       #'(lsp-describe-thing-at-point :which-key "describe symbol")
 ;; local mode/leader
 "m"       '(:ignore t :which-key "local mode")
 ;; Narrow
 "n"       '(:ignore t :which-key "notes/narrow")
 "nf"      #'(narrow-to-defun :which-key "Narrow function")
 "nr"      #'(narrow-to-region :which-key "Narrow region")
 "nw"      #'(widen :which-key "Narrow widen/remove")
 ;; Org/Other
 "o"       '(:ignore t :which-key "other")
 "oc"      #'(org-capture :which-key "org capture")
 "oo"      #'(org-open-at-point :which-key "open link at point")
 "ot"      #'org-todo-list

 "ol"      '(:ignore t :which-key "org-link")
 "oli"     #'org-insert-link
 "ols"     #'org-store-link

 "oO"      '(:ignore t :which-key "other")
 "oOd"     #'(diego/delete-last-char-eol :which-key "Delete last char EOL")

 "os"     #'outline-show-entry
 "oS"     #'outline-show-all
 "oh"     #'outline-hide-entry
 "oH"     #'outline-hide-body

 ;; Project
 "p"       '(:ignore t :which-key "project")
 "!"       #'(project-shell-command :which-key "Run cmd in project root")
 ;; "pb"      #'(project-switch-to-buffer :which-key "Project buffer")
 "pb"      #'(consult-project-buffer :which-key "Project buffer")
 "pf"      #'(project-find-file :which-key "Project file")
 "pp"      #'(project-switch-project :which-key "Switch project")
 "pr"     #'(diego/consult-buffer-for-project :which-key "Project recent")
 "pt"     #'diego/project-generate-ctags
 "pv"      #'(diego/vterm-project :which-key "Project vterm")
 ;; Search
 "s"       '(:ignore t :which-key "search")
 "sd"      #'(consult-find :which-key "Search file names")
 "sg"      #'(consult-grep :which-key "Search with rg")
 "ss"      #'(consult-line :which-key "Search lines")
 "sS"      #'(diego/search-symbol-at-point :which-key "Search at point")
 "sp"      #'(consult-ripgrep :which-key "Search in project")
 ;; Toggle
 "t"       '(:ignore t :which-key "toggle")
 "tf"      #'toggle-frame-fullscreen
 "th"      #'(hs-toggle-hiding :which-key "Hide/Show block")
 "tH"      #'(diego/toggle-hideshow-all :which-key "Hide/Show All")
 "tn"      #'(global-display-line-numbers-mode :which-key "Line numbers")
 "tt"      #'toggle-truncate-lines
 "tT"      #'(consult-theme :which-key "Toggle theme")
 "tv"      #'mixed-pitch-mode ; variable-pitch-mode
 "tw"      #'whitespace-mode
 "tW"      #'(visual-line-mode :which-key "Soft line wrapping")
 "t TAB"   #'(diego/toggle-workspaces-enabled :which-key "Toggle tabs as workspaces")
 ;; Quit/Restart
 "q"       '(:ignore t :which-key "quit/restart")
 "qf"      #'(delete-frame :which-key "Delete frame")
 "qr"      #'restart-emacs
 "qq"      #'(kill-emacs :which-key "Quit Emacs")
 ;; Replace
 "r"       '(:ignore t :which-key "replace")
 "ri"      #'(iedit-mode :which-key "iedit") ; next item TAB
 "rs"      #'replace-string
 "rr"      #'replace-regexp
 "rw"      #'fixup-whitespace
 ;; Window manipulation
 "w"       '(:ignore t :which-key "windows")
 "wa"      #'ace-window
 "wb"      #'balance-windows
 "wB"      #'balance-windows-area
 "ws"      #'diego/swap-windows
 "ww"      #'diego/dedicated-mode
 "w-"      #'(+evil/window-split-and-follow :which-key "Horizontal split window")
 "w/"      #'(+evil/window-vsplit-and-follow :which-key "Vertical split window")
 "wd"      #'(evil-window-delete :which-key "Close window")
 "wD"      #'ace-delete-window

 "we"      '(:ignore t :which-key "enlarge")
 "wel"     #'enlarge-window-horizontally
 "weh"     #'shrink-window-horizontally

 "wf"      #'fit-window-to-buffer
 "wF"      #'diego/follow-mode-3
 "wh"      #'(evil-window-left :which-key "Window left")
 "wj"      #'(evil-window-down :which-key "Window down")
 "wk"      #'(evil-window-up :which-key "Window up")
 "wl"      #'(evil-window-right :which-key "Window right")
 "wm"      #'(delete-other-windows :which-key "Maximize window")
 "wx"      #'diego/window-remove-side-parameter
 "wz"      #'tab-bar-history-back
 "wr"      #'tab-bar-history-forward
 )


;;* Functions

;; elisp
(load-file (substitute-in-file-name "$EMACS_ADDITIONAL_DIR/shopify-vars.el"))
(load (expand-file-name "diego-common.el" user-emacs-directory))
(load (expand-file-name "diego-windows.el" user-emacs-directory))


;;* Work Specific Functions

;; elisp
(load-file (substitute-in-file-name "$EMACS_ADDITIONAL_DIR/emacs.el"))
(load (expand-file-name "diego-dev.el" user-emacs-directory))


;;* Test In Progress

;;** vale

;; https://emacstil.com/til/2022/03/05/setting-up-vale-prose-linter-on-emacs/


(flycheck-define-checker vale
  "A checker for prose"
  :command ("vale"  "--output" "line" source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode gfm-mode org-mode text-mode))

(add-to-list 'flycheck-checkers 'vale 'append)


;;** codereview



(use-package code-review)

;;** org-modern

(use-package org-modern
  :straight (org-modern :host github :repo "minad/org-modern")
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e nil
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…")

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))



;;** tree-sitter


(use-package tree-sitter
  :straight (tree-sitter :host github :repo "emacs-tree-sitter/elisp-tree-sitter")
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs :host github :repo "emacs-tree-sitter/tree-sitter-langs")
  :after tree-sitter)

;; #+begin_example elisp
;; (defun configure-imenu-Custom ()
;;   (setq imenu-generic-expression
;;         '(("Faces" "^\\(?:Show\\|Hide\\) \\(.*\\) face: \\[sample\\]" 1)
;;           ("Variables" "^\\(?:Show Value\\|Hide\\) \\([^:\n]*\\)" 1))))

;; (add-hook 'Custom-mode-hook #'configure-imenu-Custom)


;;; init.el ends here
