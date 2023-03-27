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


;; basic settings
(setq user-full-name "Diego Alvarez")
(setq user-mail-address "diego.canada@icloud.com")
(setq use-package-hook-name-suffix nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

;; ** Authinfo
(setq auth-sources '("~/.authinfo.gpg"))
;; ** Disable exit confirmation
(setq confirm-kill-emacs nil)
;; ** Use short answers y/n
;; y-or-n-p  uses shorter answers "y" or "n".
(setq use-short-answers t) ; new in Emacs28
;; ** Server mode
;; Start the Emacs server from this instance so that all =emacsclient= calls are routed here.
(add-hook 'after-init-hook #'server-start)
;; Fix `with-editor: Cannot determine a suitable Emacsclient` issue.
(setq-default with-editor-emacsclient-executable "emacsclient")
;; ** Increase read output from processes
;; Increase how much is read from processes in a single chunk (default is 4kb). LSP is improved by increasing this value.
(setq read-process-output-max (* 1024 1024)) ; 1mb
;; ** Tramp
(setq tramp-verbose 2)

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

;; ** Configure PATH on macOS
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("EMACS_ADDITIONAL_DIR"
                                         "FZF_CTRL_T_COMMAND"
                                         "FZF_DEFAULT_COMMAND"
                                         "FZF_DEFAULT_OPTS"
                                         "KUBECONFIG"
                                         "MANPATH"
                                         "PATH"
                                         "USE_GKE_GCLOUD_AUTH_PLUGIN"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;; Packages

(add-to-list 'load-path (locate-user-emacs-file "modules"))
(add-to-list 'load-path (substitute-in-file-name "$EMACS_ADDITIONAL_DIR"))

(require 'diego-common)
(require 'shopify-emacs)
(require 'diego-ui)
(require 'diego-ui-font)
(require 'diego-ui-theme)
(require 'diego-scratch)
(require 'diego-mode-line)
(require 'diego-workspaces)
(require 'diego-window)
(require 'diego-tab-bar)
(require 'diego-files)
(require 'diego-editor)
(require 'diego-evil)
(require 'diego-git)
(require 'diego-project)
(require 'diego-completion)
(require 'diego-search-jump)
(require 'diego-dev)
(require 'diego-programming)
(require 'diego-productivity)
(require 'diego-vterm)
(require 'diego-org)
(require 'diego-misc)
(require 'diego-keybindings)

(modus-themes-select 'modus-vivendi)

;;; init.el ends here
