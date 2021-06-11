;;; Startup optimizations

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)
(setq garbage-collection-messages t)    ; indicator of GC activity

;; don't try to adjust frame size when UI elements changes
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable `package' in favor of `straight'.
(setq package-enable-at-startup nil)

;; The rest of this file sets up straight.el so that packages are installed by
;; it rather than built-in package.el.

;; Use "develop" rather than default "master" to use bleeding edge version.
(setq straight-repository-branch "develop")

;; I don't need the repo's history
(setq straight-vc-git-default-clone-depth 1)

;; Since byte-code is rarely compatible across different versions of Emacs, it's best we build them
;; in separate directories, per emacs version.
(setq straight-build-dir (format "build-%s" emacs-version))

(setq use-package-always-defer nil) ; I enable it and things are not loading correctly

;; Enable this (default is nil) so that (use-package ...) will use straight.
(setq straight-use-package-by-default t)

;; Default value is '(find-at-startup find-when-checking)
(setq straight-check-for-modifications '(find-at-startup find-when-checking))

;; Default is 'full which means to clone complete history.
(setq straight-vc-git-default-clone-depth 1)

;; 'https is the default
(setq straight-vc-git-default-protocol 'https)

;; Following are standard setup lines from https://github.com/raxod502/straight.el.git
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package first and setup so that use-package will use straight.el rather than
;; package.el by default.
(straight-use-package 'org)
(straight-use-package 'use-package)
(straight-use-package 'general)
(require 'org)
(require 'use-package)
(require 'general)
