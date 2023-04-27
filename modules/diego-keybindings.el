;;* Keybindings

(use-package emacs
  :straight (:type built-in)
  :after evil
  :config
;;** Simplify Leader Bindings (general.el)
;; [[https://github.com/noctuid/general.el][general.el]] allows us to set keybindings.
;; =general.el= is installed in early-init.el.*
;;** Bindings
;; ** Global Keybindings
;; repeat last macro with Q, use macro and then Q to repeat it
(keymap-set evil-normal-state-map "Q" #'evil-execute-last-recorded-macro)

;; fixes binding for corfu
(keymap-set evil-insert-state-map "C-n" nil)
(keymap-set evil-insert-state-map "C-p" nil)

(evil-define-key 'visual global-map (kbd ">") 'my/evil-shift-right)
(evil-define-key 'visual global-map (kbd "<") 'my/evil-shift-left)

;; ESC Cancels All
(define-key global-map [escape] #'keyboard-escape-quit)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "C-M-o" #'ace-window)

(keymap-global-set "s-b" #'consult-buffer)
(keymap-global-set "s-r" #'consult-recent-file)
(keymap-global-set "s-w" #'delete-window)
(keymap-global-set "M-c" #'evil-commentary-line)

(keymap-global-set "C-M-h" #'previous-buffer)
(keymap-global-set "C-M-l" #'next-buffer)

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

(keymap-global-set "M-<backspace>" #'my-backward-delete-word)

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
 "X"       #'diego/make-scratch
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
 "bl"      #'(mode-line-other-buffer :which-key "Switch to last buffer")
 "br"      #'revert-buffer-quick
 "bz"      #'bury-buffer

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
 "fp"      #'((lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :which-key "Edit private config")
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
 "gS"      #'(project-vc-dir :which-key "vc-dir project status")
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
 "ic"      #'cape-ispell
 "ie"      #'emoji-search
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
 "jc"      #'browser-hist-search
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
 "kK"      #'(diego/kill-buffer :which-key "Kill current buffer")
 "kh"      #'((lambda () (interactive) (kill-matching-buffers "\\*helpful" nil t)) :which-key "Kill help buffers")
 "kv"      #'((lambda () (interactive) (kill-matching-buffers "\\*vterm" nil t)) :which-key "Kill vterm buffers")
 "k TAB"    #'(tab-bar-close-tab :which-key "Kill workspace tab")
 "kw"      #'(kill-buffer-and-window :which-key "Kill current buffer and window")
 ;; LSP
 "l"       '(:ignore t :which-key "lsp")
 "la"      #'xref-find-apropos
 "le"      #'flycheck-list-errors
 "lk"      #'eldoc-print-current-symbol-info
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
 "wz"      #'winner-undo
 "wr"      #'winner-redo))

(provide 'diego-keybindings)
