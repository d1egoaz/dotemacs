;;; early-init.el --- Early Init File                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Diego Alvarez

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

;;; Startup optimizations

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)
(setq garbage-collection-messages t)    ; indicator of GC activity

(setq inhibit-automatic-native-compilation nil)
(setq native-comp-always-compile t)
(setq native-comp-async-jobs-number 4)
(setq native-comp-async-query-on-exit t)
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-deferred-compilation-deny-list nil)
(setq native-comp-verbose 0)
(setq package-native-compile t)

;; Disable `package' in favor of `straight'.
(setq package-enable-at-startup nil)

;; don't try to adjust frame size when UI elements changes
(setq frame-inhibit-implied-resize t)

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
(require 'use-package) ;; it's now part of emacs
(straight-use-package 'general)
(require 'general)

;;; early-init.el ends here
