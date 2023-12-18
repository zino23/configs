;; init.el --- startup file for emacs -*- lexical-binding: t -*-
;;
;;; commentary:
;; here is some magic that can grow hands on your mind, and vice versa.

;;; code:
(setq load-prefer-newer t)

;; Take the full control, don't load `default.el'
(setq inhibit-default-init t)

(require 'package)

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Native compilation is available"))
  (message "Native complation is *not* available"))

(setq native-comp-jit-compilation t)
(setq native-comp-async-report-warnings-errors nil)

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(setq user-emacs-directory "~/.config/emacs/")

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Must be set before loading `use-package'
(setq use-package-enable-imenu-support t)

(use-package use-package
  :custom
  (use-package-always-ensure t))

;; Try `straight'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package use-package-ensure-system-package)

(setenv "path" (concat "/library/tex/texbin" (getenv "path")))
(setq exec-path (append '("/Library/TeX/texbin") exec-path))

;; On macos, treat option key as meta and command as super
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

(setq confirm-kill-emacs 'yes-or-no-p)
(setq read-process-output-max (* 1024 1024)) ;; 1MB

(set-default-coding-systems 'utf-8)
(setq inhibit-startup-screen t)
(size-indication-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode nil)
(set-fringe-mode '(7 . 0))
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode -1)

;; Dynamically adjust width
(setq display-line-numbers-width nil)

;; Display line numbers in mode line
(line-number-mode 1)
(setq help-window-select t)

;; Treat manual buffer switching the same as programmatic switching
(setq switch-to-buffer-obey-display-actions t)

(setq custom-file "~/.config/emacs/init.el")

(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 300 1024 1024))

(use-package emacs
  ;; Performance tuning
  :config
  (setq-default bidi-display-reordering t)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  (setq fast-but-imprecise-scrolling t))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  :hook
  (minibuffer-setup . cursor-intangible-mode)
  (next-error . recenter)
  :custom
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate 'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Enable recursive minibuffers
  (enable-recursive-minibuffers t)
  (x-stretch-cursor t)
  (prettify-symbols-unprettify-at-point 'right-edge)
  (pulse-delay 0.04)
  (pulse-iterations 15)
  (indicate-buffer-boundaries t)
  (indicate-empty-lines nil)
  (echo-keystrokes 0.01)
  (scroll-margin 0 "needed to work well with `ultra-scroll-mac-mode'")
  (scroll-preserve-screen-position 'always)
  (scroll-error-top-bottom nil)
  (next-screen-context-lines 2)
  ;; Scroll one line at a time (less "jumpy" than defaults)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  ;; Don't accelerate scrolling
  (mouse-wheel-progressive-speed nil)
  ;; Keyboard scroll one line at a time
  (scroll-step 0)
  ;; Never recenter when scrolling off-screen
  (scroll-conservatively 10000)
  ;; Disable the visible bell
  (visible-bell nil)
  (next-error-found-function 'next-error-quit-window)
  (next-error-highlight t)
  (next-error-highlight-no-select t)
  (next-error-message-highlight t)
  :custom-face
  (variable-pitch ((t (:height 1.2 :family "Bookerly")))))

(use-package emacs
  :config
  (defun zino/toggle-window-dedication ()
    "Toggle window dedication in the selected window."
    (interactive)
    (set-window-dedicated-p (selected-window)
                            (not (window-dedicated-p (selected-window))))
    (if (window-dedicated-p (selected-window))
        (message "The current window is dedicated")
      (message "The current window is not dedicated")))
  (defun zino/toggle-scratch ()
    "Toggle `scratch' buffer."
    (interactive)
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))

  (defun delete-to-end-of-buffer ()
    (interactive)
    (kill-region (point) (point-max)))
  (add-function :before after-focus-change-function 'garbage-collect)

  :bind
  ("C-S-d" . zino/toggle-window-dedication)
  ("C-c C-z" . delete-to-end-of-buffer)
  ("C-c r" . zino/toggle-scratch))

(use-package desktop
  :ensure nil
  :config
  (setq desktop-auto-save-timeout 60)
  (desktop-save-mode 1)

  ;; Restore frame name when read desktop file
  (push '(name . nil) frameset-filter-alist)
  :config
  (unless (display-graphic-p)
    ;; https://emacs.stackexchange.com/a/45829/37427
    (setq desktop-restore-forces-onscreen nil)
    (add-hook 'desktop-after-read-hook
              (lambda ()
                (frameset-restore
                 desktop-saved-frameset
                 :reuse-frames (eq desktop-restore-reuses-frames t)
                 :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
                 :force-display desktop-restore-in-current-display
                 :force-onscreen desktop-restore-forces-onscreen)))))

(setq save-silently t)

;; Keep buffers up to date
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Save minibuffer's history inputs
(setq enable-recursive-minibuffers t)
(setq history-length 25)
(savehist-mode t)
(recentf-mode t)

;; Delete files into trashbin
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Define a read-only directory class
(dir-locals-set-class-variables 'read-only '((nil . ((buffer-read-only . t)))))

(global-set-key (kbd "s-D") nil)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-k") 'kill-current-buffer)
(global-set-key (kbd "s-u") 'revert-buffer)
(global-set-key (kbd "s--") 'text-scale-adjust)
(global-set-key (kbd "s-=") 'text-scale-adjust)
;; `m' for make
(global-set-key (kbd "s-m") 'make-frame)
(global-set-key (kbd "s-[") 'backward-paragraph)
(global-set-key (kbd "s-]") 'forward-paragraph)

;; Easy-to-access previous/next-logical-line
(global-set-key (kbd "C-S-p") 'previous-logical-line)
(global-set-key (kbd "C-S-n") 'next-logical-line)

;; The good old pager-like scrolling.
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)
;; On HHKB, <Scroll_Lock> is also <f14>
(global-set-key (kbd "<f14>") 'scroll-lock-mode)

(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)
(global-set-key (kbd "s-3") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "s-4") 'kmacro-end-or-call-macro)

(global-set-key (kbd "s-b") 'zino/switch-other-buffer)

(global-set-key (kbd "M-s-3") (lambda ()
                                "Quickly create 3 balanced vertically spit windows."
                                (interactive)
                                (split-window-right)
                                (split-window-right)
                                (balance-windows)))

(global-set-key (kbd "M-s-s") 'window-toggle-side-windows)

(define-key minibuffer-mode-map (kbd "S-SPC") nil)

(global-set-key (kbd "s-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(defun zino/toggle-window-split ()
  "Toggle between vertical and horizontal split when therea are only two window."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'zino/toggle-window-split)

;; The following only undoes remapping instead of remapping to nil
;; (define-key global-map [remap ns-print-buffer] nil)
(global-unset-key (kbd "s-p"))

;; the original keybinding is set-goal-column
(global-unset-key (kbd "C-x C-n"))

(global-set-key (kbd "C-c +") 'zino/increment-number-decimal)

(global-set-key (kbd "C-d") 'zino/delete-forward-char)

(defun zino/delete-forward-char (n)
  "Delete the following N chars and keep it in the `kill-ring'."
  (interactive "p*")
  (delete-char n t))

;; vim like newline
(defun newline-below ()
  "Newline like vim's o."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun newline-above ()
  "Newline like vim's O."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))

(global-set-key (kbd "C-S-<return>") 'newline-above)
(global-set-key (kbd "S-s-<return>") 'newline-above)
(global-set-key (kbd "s-<return>") 'newline-below)
(global-set-key (kbd "<C-return>") 'newline-below)

(defun zino/next-k-lines ()
  "Move cursor down k lines."
  (interactive)
  (scroll-up 5))

(defun zino/previous-k-lines ()
  "Move cursor up k lines."
  (interactive)
  (scroll-down 5))

(global-set-key (kbd "M-n") 'zino/next-k-lines)
(global-set-key (kbd "M-p") 'zino/previous-k-lines)
;; (global-set-key (kbd "C-,") 'scroll-up-command)
;; (global-set-key (kbd "C-.") 'scroll-down-command)

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-e") 'forward-word)

(defun zino/kill-whole-line-without-newline ()
  "Kill the whole line but leave the trailing newline."
  (interactive)
  (kill-whole-line 0))

(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-S-k") 'kill-line)
(global-set-key (kbd "C-S-s-k") 'zino/kill-whole-line-without-newline)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((old-point (point)))
    (beginning-of-visual-line)
    (skip-syntax-forward " " (line-end-position))
    (backward-prefix-chars)
    (when (= old-point (point))
      (beginning-of-visual-line))))

(defun copy-line (arg)
  "Copy ARG lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion
                      (goto-char (mark))
                      (line-beginning-position)))
        (setq end (save-excursion
                    (goto-char (mark))
                    (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  ;; move cursor to the next line of the last of copied lines
  (beginning-of-line (or (and arg (1+ arg)) 2))
  ;; print a message when copying more than 1 lines
  (if (and arg (not (= 1 arg)))
      (message "%d lines copied" arg)))

(global-set-key (kbd "C-c k") 'copy-line)
(global-set-key (kbd "C-c C-k") 'copy-line)
(defun zino/switch-other-buffer ()
  "Swithch to the most recent buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "C-c b") 'zino/switch-other-buffer)
(global-set-key (kbd "C-c C-b") 'zino/switch-other-buffer)

(global-set-key (kbd "<RET>") 'newline)
(defun zino/save-buffer-and-exit()
  "Simple convenience function.
Save the buffer of the current window and kill it"
  (interactive)
  (save-buffer)
  (delete-window))
(global-set-key (kbd "C-x s-s") #'zino/save-buffer-and-exit)

;; "C-@" is used in rime to toggle between English and Chinese
(global-unset-key (kbd "C-@"))

;; The original 'C-@' is hard to type.
(global-set-key (kbd "C-M-x") 'mark-sexp)

;; Emacs source for help system
;; (setq emacs-src-dir "/usr/local/Cellar/emacs-plus@29/29.0.60/share/emacs")
;; (setq macos-sdk-dir "/Applications/Xcode.app/Contents/Develop er/Platforms/MacOSX.platform/Developer/SDKs/MacOSX11.1.sdk")
;; (setq go-src-dir "/usr/local/Cellar/go/1.17.8/libexec/src")
;; (setq emacs-pkgs-dir "~/.config/emacs/elpa")
;; (setq rust-src-dir "~/.rustup/toolchains")

;; Associate directories with the read-only class
;; (dolist (dir (list
;;               "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"
;;               emacs-src-dir
;;               go-src-dir
;;               rust-src-dir
;;               "~/.rustup/toolchains/stable-x86_64-apple-darwin"))
;;   (dir-locals-set-directory-class (file-truename dir) 'read-only))

;; A better way: add ((nil . ((buffer-read-only . t)))) in `.dir-locals.el' at
;; the directory whose files should be opened in read-only buffers.

;; https://emacs.stackexchange.com/a/7670/37427
;; Edebug a defun or defmacro
(defvar zino/fns-in-edebug nil
  "List of functions for which `edebug' is instrumented.")

(defconst zino/fns-regexp (concat "(\\s-*"
                                  "\\(defun\\|defmacro\\)\\s-+"
                                  "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
  "Regexp to find defun or defmacro definition.")

(defun zino/toggle-edebug-defun ()
  (interactive)
  (let (fn)
    (save-excursion
      (search-backward-regexp zino/fns-regexp)
      (setq fn (match-string 1))
      (mark-sexp)
      (narrow-to-region (point) (mark))
      (if (member fn zino/fns-in-edebug)
          ;; If the function is already being edebugged, uninstrument it
          (progn
            (setq zino/fns-in-edebug (delete fn zino/fns-in-edebug))
            (eval-region (point) (mark))
            (setq-default eval-expression-print-length 12)
            (setq-default eval-expression-print-level  4)
            (message "Edebug disabled: %s" fn))
        ;; If the function is not being edebugged, instrument it
        (progn
          (add-to-list 'zino/fns-in-edebug fn)
          (setq-default eval-expression-print-length nil)
          (setq-default eval-expression-print-level  nil)
          (edebug-defun)
          (message "Edebug: %s" fn)))
      (widen))))

(use-package windmove
  :ensure nil
  :bind
  ("s-;" . windmove-display-new-frame)
  ("s-'" . windmove-display-same-window)
  ("s-<left>" . windmove-display-left)
  ("s-<right>" . windmove-display-right)
  ("s-<up>" . windmove-display-up)
  ("s-<down>" . windmove-display-down)
  :custom
  (windmove-wrap-around t)
  :init
  (windmove-default-keybindings))

(use-package frame
  :ensure nil
  :config
  (define-prefix-command 'frame-map)
  :bind
  ("C-c f" . frame-map)
  ("C-c f r" . set-frame-name)
  ("C-c f s" . select-frame-by-name)
  ("C-M-<tab>" . select-frame-by-name))

(use-package elisp-mode
  :ensure nil)

(use-package eros
  :hook
  (emacs-lisp-mode . eros-mode))

;; `auto-fill-mode' is defined in package `simple'
;; Auto soft-break line
(use-package simple
  :ensure nil
  :config
  (auto-fill-mode nil)
  :hook
  ;; To toggle automatic line breaking, M-x `auto-fill-mode'
  (prog-mode . (lambda ()
                 "Set `auto-fill-mode' for comments except for `emacs-lisp-mode'."
                 (auto-fill-mode 1)
                 (setq-local comment-auto-fill-only-comments (if (not (derived-mode-p 'emacs-lisp-mode)) t)
                             fill-column (if (derived-mode-p 'emacs-lisp-mode) 110 80)))))

(global-visual-line-mode 1)

(use-package hl-line
  :ensure nil
  :init
  (global-hl-line-mode)
  :config
  (add-hook 'vterm-mode-hook (lambda ()
                               "Disable `global-hl-line-mode' locally."
                               (setq-local global-hl-line-mode nil))))

;; Word abbreviation
;; "C-x a g" to interactively create an abbrev;
;; "C-u - C-x a g" to remove one.
(use-package abbrev
  :ensure nil
  :preface
  (defun self-insert-no-abbrev ()
    (interactive)
    (let ((abbrev-mode nil))
      (call-interactively 'self-insert-command)))
  :init
  (setq-default abbrev-mode t)
  :bind
  ("_" . self-insert-no-abbrev)
  ("," . self-insert-no-abbrev)
  ("." . self-insert-no-abbrev)
  ("-" . self-insert-no-abbrev)
  ("\"" . self-insert-no-abbrev)
  (";" . self-insert-no-abbrev)
  (":" . self-insert-no-abbrev)
  ("(" . self-insert-no-abbrev)
  (")" . self-insert-no-abbrev)
  ("/" . self-insert-no-abbrev)
  ("`" . self-insert-no-abbrev)
  ("=" . self-insert-no-abbrev)
  ("<" . self-insert-no-abbrev)
  (">" . self-insert-no-abbrev)

  :custom
  (abbrev-file-name "~/.config/emacs/abbrev_defs")
  (save-abbrevs 'silently)
  (abbrev-suggest nil)
  (abbrev-suggest-hint-threshold 3)
  :config
  (quietly-read-abbrev-file)
  (abbrev-table-put global-abbrev-table :case-fixed t)

  (progn
    (setq zino/abbrev-table '(("tset" "test")
                              ("arch" "architecture")
                              ("conf" "configuration")
                              ("tyep" "type")
                              ("env" "environment")
                              ("fiel" "file")
                              ("recieve" "receive")
                              ("sturct" "struct")
                              ("adn" "and")
                              ("teh" "the")
                              ("JS" "JavaScript")
                              ("arch" "architecture")
                              ("hte" "the")
                              ("wiat" "wait")
                              ("smae" "same")
                              ("accordign" "according")
                              ("lgo" "log")
                              ("taht" "that")
                              ("contorl" "control")))
    (dolist (pair zino/abbrev-table)
      (let ((from (car pair))
            (to (car (cdr pair))))
        (define-abbrev global-abbrev-table from to)))))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(use-package ibuffer
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 80 80 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 -1 :left :elide)
           " "
           (filename-and-process 16 70 :left :elide))
     (mark " "
           (name 80 -1)
           " " filename)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Ibuffer\\*" display-buffer-full-frame))
  :bind
  ("C-x C-b" . ibuffer)
  (:map ibuffer-mode-map
        ("q" . (lambda ()
                 (interactive)
                 (quit-window)
                 (let ((inhibit-message t))
                   (winner-undo))))))

(use-package dired
  :ensure nil
  :commands
  (dired dired-jump)
  :bind
  (("C-x C-j" . dired-jump)
   :map dired-mode-map
   ("(" . dired-hide-details-mode)
   ("<backspace>" . dired-up-directory))
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-hide-details-mode)
  ;; Omit uninterested files
  (dired-mode . dired-omit-mode)
  ;; Drag-and-drop to `dired'
  (dired-mode . org-download-enable)
  :custom
  (dired-dwim-target t)
  (dired-create-destination-dirs 'always)
  (dired-listing-switches "-alGuh --group-directories-first")
  (auto-revert-verbose nil)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-omit-verbose nil)
  :config
  (setq ls-lisp-use-insert-directory-program t)
  ;; `ls' does not have `--group-directories-first' option, use `gls'.
  (setq insert-directory-program "/usr/local/bin/gls"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :init
  (diredfl-global-mode))

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  ;; `ibuffer-formats' does not take effects when `nerd-icons-ibuffer-mode' is on. These numbers are fine-tuned
  ;; to fit nicely in a 27" monitor.
  (nerd-icons-ibuffer-formats
   '((mark modified read-only locked " "
           (icon 2 2)
           (name 80 80 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode+ 16 -1 :left :elide)
           " "
           (filename-and-process 16 120 :left :elide))
     (mark " "
           (name 80 -1)
           " " filename))))

(use-package dired-preview
  ;; Introduce too much sluggish
  :disabled
  :init
  (dired-preview-global-mode)
  :custom
  (dired-preview-delay 0.1)
  :bind
  (:map dired-mode-map
        ("C-x M-p" ("make keybinding align with `dired-omit-mode' 'C-x M-o'" . dired-preview-mode))))

(use-package dired-rsync
  :bind
  (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

;; (use-package dirvish
;;   :disabled
;;   :init
;;   ;; `dirvish' cause sluggish in `magit-status'. Figure out the reason later
;;   ;; (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
;;    '(("h" "~/"                    "Home")
;;      ("r" "/"                     "Root")
;;      ("n" "~/Notes/"              "Notes")
;;      ("c" "~/Documents/Books/CS/" "CS")
;;      ("d" "~/Downloads/"          "Downloads")
;;      ("m" "/mnt/"                 "Drives")
;;      ("t" "~/.Trash/"             "TrashCan")))
;;   :config
;;   ;; (dirvish-peek-mode) ; Preview files in minibuffer
;;   ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
;;   (setq dirvish-mode-line-format
;;         '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes
;;         '(file-time file-size collapse subtree-state vc-state git-msg))
;;   (setq delete-by-moving-to-trash t)
;;   (setq dired-listing-switches
;;         "-l --almost-all --human-readable --group-directories-first --no-group")
;;   :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
;;   (("C-c f" . dirvish-fd)
;;    ("C-c C-j" . dirvish-side)
;;    :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
;;    ("a"   . dirvish-quick-access)
;;    ("f"   . dirvish-file-info-menu)
;;    ("y"   . dirvish-yank-menu)
;;    ("N"   . dirvish-narrow)
;;    ("^"   . dired-up-directory)
;;    ("l"   . dirvish-history-last)
;;    ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;;    ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;;    ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;;    ("TAB" . dirvish-subtree-toggle)
;;    ("M-f" . dirvish-history-go-forward)
;;    ("M-b" . dirvish-history-go-backward)
;;    ("M-l" . dirvish-ls-switches-menu)
;;    ("M-m" . dirvish-mark-menu)
;;    ("M-t" . dirvish-layout-toggle)
;;    ("M-s" . dirvish-setup-menu)
;;    ("M-e" . dirvish-emerge-menu)
;;    ("M-j" . dirvish-fd-jump)))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-idle-delay 0.05)
  (eldoc-echo-area-prefer-doc-buffer t)
  (max-mini-window-height 0.2))

(use-package eldoc-box
  :bind
  ("M-." . eldoc-box-help-at-point))

(defun copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring."
  (interactive)
  (when buffer-file-name (kill-new (file-truename buffer-file-name))
        (message buffer-file-name)))

;; Full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Natual Title Bar is disabled by default. Enable it by setting:
;; defaults write org.gnu.Emacs TransparentTitleBar DARK
;; Reference: https://github.com/railwaycat/homebrew-emacsmacport/wiki/Natural-Title-Bar
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (setq ns-use-proxy-icon nil)
;; (setq frame-title-format nil)

;; Check if a font exist:
;; (member "Sarasa Mono SC Nerd" (font-family-list))

;; Set up fonts for different charsets.
(defun zino/set-font (font-name cn-font-name &optional initial-size cn-font-rescale-ratio)
  "Set different font-family for Latin and Chinese charactors."
  (let* ((size (or initial-size 14))
         (ratio (or cn-font-rescale-ratio 0.0))
         (main (font-spec :name font-name :size size))
         (cn (font-spec :name cn-font-name)))
    (set-face-attribute 'default nil :font main)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset cn))
    (setq face-font-rescale-alist (if (/= ratio 0.0) `((,cn-font-name . ,ratio)) nil))))

(zino/set-font "Fira Code" "Sarasa Mono SC Nerd" 14 1)

;; Automatically trim trailing whitespaces.
(use-package ws-butler
  :config
  (ws-butler-global-mode))

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook shell-mode-hook eshell-mode-hook pdf-view-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 0))))

;; Format code while editing lisp code
(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package rainbow-mode
  :hook
  (emacs-lisp-mode . rainbow-mode)
  (lispy-data-mode . rainbow-mode)
  (helpful-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (lispy-data-mode . rainbow-delimiters-mode)
  (debugger-mode . rainbow-delimiters-mode)
  (helpful-mode . rainbow-delimiters-mode)
  (dired-preview-mode . rainbow-delimiters-mode)
  (conf-toml-mode . rainbow-delimiters-mode))

;; Display keybindings in another buffer
(use-package command-log-mode
  :config
  (global-command-log-mode -1)
  :bind
  ("C-c C-o" . clm/toggle-command-log-buffer)
  :custom
  (command-log-mode-window-font-size 3))

(use-package keycast
  :config
  (setq keycast-mode-line-insert-after '(:eval (doom-modeline-format--main)))
  (add-to-list 'global-mode-string '("" keycast-mode-line)))

(use-package paren
  :config
  (show-paren-mode 1)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed)
  :custom-face
  (show-paren-match-expression ((t (:inherit nil :background "#282c34" :weight bold)))))

(use-package nerd-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 24)
  (doom-modeline-indent-info nil)
  (doom-modeline-hud nil)
  (doom-modeline-unicode-fallback t)
  ;; Whether display the modal state icon.
  ;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
  (doom-modeline-modal-icon t)
  (doom-modeline-highlight-modified-buffer-name t)
  :custom-face
  ;; (doom-modeline-buffer-modified ((t (:background unspecified :inherit (warning bold)))))
  (doom-modeline-buffer-modified ((t (:background unspecified :inherit (bold))))))

;; (use-package spaceline
;;   :config
;;   (spaceline-spacemacs-theme))

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  )

(use-package emojify
  :hook
  (erc-mode . emojify-mode)
  :commands
  emojify-mode)

(use-package undo-tree
  ;; `undo-tree' is not fast enough for me on mac.
  :disabled
  :init
  (global-undo-tree-mode t)
  :custom
  (undo-tree-auto-save-history nil))

(use-package smartparens
  :bind
  (;; not smartparens related, but shares the same philosophy
   ("C-s-f" . beginning-of-defun)
   ("C-s-g" . end-of-defun)

   ("C-s-a" . sp-beginning-of-sexp)
   ("C-s-e" . sp-end-of-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-e" . sp-up-sexp)
   ("C-M-a" . sp-backward-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)

   ;; in the same nested level and go up a level if needed
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("M-r" ("unwrap the enclosing sexp" . sp-splice-sexp))
   ("M-k" ("delete anything of the enclosing sexp". sp-change-enclosing))
   ("C-M-r" ("rewrap with a different pair". sp-rewrap-sexp))
   ("M-y" ("copy the sexp at point". sp-copy-sexp))
   ("C-M-k" . sp-kill-sexp)
   ("C-c s b" ("remove the last sexp from current list by moving the closing delimiter" . sp-forward-barf-sexp))
   ("C-c s s" ("eat the next sexp into current one". sp-forward-slurp-sexp))
   ("C-c s n" ("unwrap the next sexp. 'n' as in 'next'". sp-unwrap-sexp))
   ("C-c s c" ("unwrap the current list. 'c' as in 'current'". sp-splice-sexp))
   ("C-c s k" ("clear the inside of the enclosing sexp, like vim's <ci)>". sp-change-enclosing)))

  :config
  (smartparens-global-mode)

  ;; Handle apostrophe and single quote in Lisp mode
  (require 'smartparens-config)

  (global-set-key (kbd "C-c (")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
  (global-set-key (kbd "C-c [")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
  (global-set-key (kbd "C-c {")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")))
  (global-set-key (kbd "C-c \"")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
  (global-set-key (kbd "C-c \'")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\'")))
  (global-set-key (kbd "C-c `")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "`")))

  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "{" "}" :wrap "C-{")
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))

  :bind
  (:repeat-map
   smartparens-mode-repeat-map
   ("A" . sp-beginning-of-sexp)
   ("E" . sp-end-of-sexp)
   ("f" . sp-forward-sexp)
   ("b" . sp-backward-sexp)
   ("d" . sp-down-sexp)
   ("e" . sp-up-sexp)
   ("a" . sp-backward-down-sexp)
   ("u" . sp-backward-up-sexp)
   ("n" . sp-next-sexp)
   ("p" . sp-previous-sexp)
   ("B" . sp-forward-barf-sexp)
   ("s" . sp-forward-slurp-sexp)
   ;; ("k" . sp-change-enclosing)
   ("c" . sp-splice-sexp)
   :repeat-map
   walk-defun-repeat-map
   ("f" . beginning-of-defun)
   ("g" . end-of-defun))
  :config
  (advice-add
   'sp-copy-sexp
   :after
   (lambda (&optional arg)
     "Print a message after applying `sp-copy-sexp'."
     (let ((sexp (read (car kill-ring))))
       (message (concat "sexp '" (format "%s" sexp) "' has been copied to the kill-ring")))))

  ;; :hook
  ;; `smartparens-global-mode' does not enable `smartparens-mode' in minibuffer
  ;; NOTE: disable it for now coz often I don't want quoting characters to be
  ;; automatically inserted.
  ;; (minibuffer-mode . smartparens-mode)
  )

(defmacro def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\")))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
                collect
                `(defun ,(read (concat
                                "wrap-with-"
                                (prin1-to-string key)
                                "s"))
                     (&optional arg)
                   (interactive "p")
                   (sp-wrap-with-pair ,val)))))

(def-pairs ((paren        . "(")
            (bracket      . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote   . "`")
            (star         . "*")
            (underscore   . "_")))

(use-package vertico
  :init
  (vertico-mode)

  :config
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 10)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize 'grow-only)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)

  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  :bind
  (:map vertico-map
        ;; Exit minibuffer with input instead of current candidate.
        ("C-j" . vertico-exit-input)
        ("C-M-n" . vertico-next-group)
        ("C-M-p" . vertico-previous-group)
        ;; Insert the current candidate
        ("TAB" . vertico-insert)
        ("?" . minibuffer-completion-help))
  :custom
  (vertico-sort-function 'vertico-sort-history-length-alpha))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :config
  (vertico-multiform-mode)
  :custom
  ;; Configure the display per command.
  ;; Use a buffer with indices for imenu
  ;; and a flat (Ido-like) menu for M-x.
  (vertico-multiform-commands '((consult-imenu buffer indexed))))

(use-package vertico-quick
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
        ("s-j" . vertico-quick-jump)
        ;; Exit vertico with the selected candidate
        ("C-q" . vertico-quick-exit)
        ("M-q" . vertico-quick-insert)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.2)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-show-docstrings nil)
  :custom
  (which-key-popup-type 'minibuffer))

(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ("C-c C-d" . helpful-at-point)
  (:map helpful-mode-map
        ;; Already got <tab> for `next-button'. Often times being able to
        ;; scroll the screen with single hand is nice.
        ("n" . next-line)
        ("p" . previous-line)
        ("f" . forward-char)
        ("b" . backward-char)))

(use-package projectile
  :config
  (projectile-mode)
  :custom
  ;; `alien' indexing method ignores patterns listed in .gitignore, but does not
  ;; respect .projectile
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  (projectile-globally-ignored-directories nil) ; quick fix for bbatsov/projectile#1777
  :bind-keymap
  ("s-p" . projectile-command-map)
  :bind
  ([remap projectile-switch-to-buffer] . consult-project-buffer)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev")))
  ;; [use] call `projectile-find-file' with prefix argument will invalidate cache first
  (setq projectile-switch-project-action #'projectile-find-file))

;; Boost performance of `magit'
(use-package libgit
  :load-path "~/.config/emacs/manually_installed/libegit2/"
  :custom
  (libgit-auto-rebuild t))

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ([remap vc-diff] . magit-diff-buffer-file)
   :map magit-stash-mode-map
   ("M-n" . zino/next-k-lines)
   ("M-p" . zino/previous-k-lines)
   ("M-s-n" . magit-section-forward-sibling)
   ("M-s-p" . magit-section-backward-sibling))
  :custom
  (magit-git-executable "/usr/bin/git")
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  ;; `ediff' on a hunk only shows two window. Hide the one displaying common parts.
  (magit-ediff-dwim-show-on-hunks t)
  (ediff-split-window-function 'split-window-horizontally)
  ;; [use] magit-log-buffer-file: show a region or buffer's commit history
  (magit-diff-refine-hunk t)
  (magit-diff-highlight-trailing nil)
  (magit-pre-refresh-hook nil)
  (magit-process-popup-time -1)
  ;; Only refresh the current magit buffer
  (magit-refresh-status-buffer nil)
  :config
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (dolist (section '((untracked . hide)
                     (magit-unpushed-section . show)))
    (add-to-list 'magit-section-initial-visibility-alist section))

  ;; When you initiate a commit, then Magit by default automatically shows a
  ;; diff of the changes you are about to commit. For large commits this can
  ;; take a long time, which is especially distracting when you are committing
  ;; large amounts of generated data which you don’t actually intend to
  ;; inspect before committing. This behavior can be turned off using:
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))

(use-package magit-pull
  :ensure nil
  :after magit
  :config
  (transient-insert-suffix 'magit-pull "p"
    '("F" "default" magit-fetch-from-upstream)))

(use-package magit-push
  :ensure nil
  :after magit
  :config
  (transient-insert-suffix 'magit-push "p"
    '("P" "default" magit-push-current-to-upstream)))

(use-package magit-status
  :ensure nil
  :after magit
  :config
  ;; Speed up Magit status by not generating all of the available sections.
  (dolist (func '(
                  ;; magit-insert-status-headers
                  ;; magit-insert-merge-log
                  ;; magit-insert-rebase-sequence
                  ;; magit-insert-am-sequence
                  ;; magit-insert-sequencer-sequence
                  ;; magit-insert-bisect-output
                  ;; magit-insert-bisect-rest
                  ;; magit-insert-bisect-log
                  ;; magit-insert-untracked-files
                  ;; magit-insert-unstaged-changes
                  ;; magit-insert-staged-changes
                  ;; magit-insert-stashes
                  ;; magit-insert-unpushed-to-pushremote
                  ;; magit-insert-unpushed-to-upstream-or-recent
                  ;; magit-insert-unpulled-from-pushremote
                  ;; magit-insert-unpulled-from-upstream
                  ))
    (remove-hook 'magit-status-sections-hook func)))

(use-package diff-hl
  :custom
  (diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup)
  (diff-hl-show-staged-changes nil)
  :config
  (advice-add 'diff-hl-next-hunk :after (lambda (&optional backward)
                                          (recenter))))

;; https://github.com/dandavison/magit-delta/issues/6
;; (with-eval-after-load 'magit-delta
;;   (set-face-attribute 'magit-diff-added-highlight nil
;;                       :background "#3e493d") ;; #002800
;;   (set-face-attribute 'magit-diff-added nil
;;                       :background "#3e493d")
;;   (set-face-attribute 'magit-diff-removed-highlight nil
;;                       :background "#4f343a") ;; #3f0001
;;   (set-face-attribute 'magit-diff-removed nil
;;                       :background "#4f343a"))

;; (add-hook 'magit-delta-mode-hook
;;           (lambda ()
;;             (setq face-remapping-alist
;;                   (seq-difference face-remapping-alist
;;                                   '((magit-diff-removed . default)
;;                                     (magit-diff-removed-highlight . default)
;;                                     (magit-diff-added . default)
;;                                     (magit-diff-added-highlight . default))))))

(use-package org-roam
  :custom
  (org-roam-directory "~/Notes/Roam")
  (org-roam-dailies-directory "Journal/")
  (org-roam-completion-everywhere nil)
  (org-roam-capture-templates
   '(("c" "Default" entry "* %?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${slug}\n#+FILETAGS: %^{tags}\n#+CREATED: %<%Y-%m-%d>\n#+STARTUP: fold")
      :empty-lines-before 1
      :unnarrowed nil)))
  (org-roam-dailies-capture-templates
   '(("c" "default" entry
      #'org-roam-capture--get-point
      "* %?"
      :target "Journal/%<%Y-%m-%d>"
      :head "#+title: %<%Y-%m-%d %a>\n\[[roam:%<%Y-%B>]]\n\n")
     ("m" "meeting" entry
      #'org-roam-capture--get-point
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:40}" 'face 'org-tag)))
  :config
  (org-roam-db-autosync-mode)
  (defun zino/org-roam-node-find-other-window ()
    (interactive)
    (org-roam-node-find 'other-windw))
  :bind
  ;; Keybindings containing `4' before normal key actions often means to execute
  ;; the action in other window.
  ("s-n 4 f" . zino/org-roam-node-find-other-window)
  ("s-n f" . org-roam-node-find))

(use-package magit-todos
  ;; Not compatible with Emacs 28
  :disabled
  :cusyom
  (magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'"))

(use-package magit-delta
  :hook
  (magit-mode . magit-delta-mode)
  :custom
  (magit-delta-default-dark-theme "Solarized (dark)")
  (magit-delta-hide-plus-minus-markers nil))

(use-package git-timemachine)

(use-package hl-todo
  :custom
  (hl-todo-wrap-movement t)
  (hl-todo-keyword-faces
   '(("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("MAYBE" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("TEMP" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXXX*" . "#cc9393")))
  :hook
  (prog-mode . hl-todo-mode)
  :bind
  ("M-s-[" . hl-todo-previous)
  ("M-s-]" . hl-todo-next))

(use-package git-gutter
  :bind
  (("C-c C-u g" . git-gutter-mode)
   ("C-c C-u [" . git-gutter:previous-hunk)
   ("C-c C-u ]" . git-gutter:next-hunk)
   ("C-c C-u p" . git-gutter:popup-hunk)
   ("C-c C-u s" . git-gutter:stage-hunk)
   ("C-c C-u r" . git-gutter:revert-hunk)
   ("C-c C-u m" . git-gutter:mark-hunk)
   :repeat-map
   git-gutter-repeat-map
   ("[" . git-gutter:previous-hunk)
   ("]" . git-gutter:next-hunk))
  :custom
  (git-gutter:update-interval 0.1 "Automatically update diff in 0.1 seconds")
  (git-gutter:window-width 1))

(use-package org
  :preface
  (defun org-mode-setup ()
    "Run after `org-mode' is initiated."
    (org-indent-mode)
    (set-face-attribute 'org-table nil :font (font-spec :name "Sarasa Mono SC Nerd" :size 16))
    (set-fontset-font t nil "Sarasa Mono SC Nerd" nil 'append)
    (setq-local corfu-auto-delay 0.2))

  (defun individual-visibility-source-blocks ()
    "Fold some blocks in the current buffer."
    (interactive)
    (org-show-block-all)
    (org-block-map
     (lambda ()
       (let ((case-fold-search t))
         (when (and
                (save-excursion
                  (beginning-of-line 1)
                  (looking-at org-block-regexp))
                (cl-assoc
                 ':hidden
                 (cl-third
                  (org-babel-get-src-block-info))))
           (org-hide-block-toggle))))))

  (defun zino/advise-org-edit-src-code (f &rest args)
    "Temporarily make current window undedicated if needed."
    (let ((dedicated-p (window-dedicated-p)))
      (if dedicated-p
          (progn
            (zino/toggle-window-dedication)
            (apply f args)
            (zino/toggle-window-dedication)))))

  ;; set `org-src-window-setup' to 'current-window
  ;; this will not work, tinker later
  ;; (advice-add 'org-edit-src-code :around 'zino/advise-org-edit-src-code)

  (defun zino/org-babel-tangle-rename ()
    (let ((tangle-file (buffer-file-name)))
      (rename-file tangle-file zino/org-babel-tangle-dir t)))

  (defun zino/org-edit-src-code-advice (oldfun &rest args)
    "Call OLDFUN with ARGS, then set a jump point with `better-jumper-set-jump'."
    (apply oldfun args)
    (setq-local completion-at-point-functions (remove 'cape-dabbrev completion-at-point-functions)))

  :config
  ;; `cape-dabbrev' slows Emacs down
  (advice-add 'org-edit-src-code :around 'zino/org-edit-src-code-advice)

  ;; NOTE: Enable it when we want to tangle a file, edit the file with lsp enabled,
  ;; and detangle it back to the org source block.
  (add-hook 'org-babel-post-tangle-hook 'zino/org-babel-tangle-rename)
  ;; (remove-hook 'org-babel-post-tangle-hook 'zino/org-babel-tangle-rename)

  ;; NOTE: Use absolute links to make `org-babel-detangle' work when the filename of
  ;; the tangled file is transformed in `org-babel-post-tangle-hook', namely
  ;; `zino/org-babel-tangle-dir'.
  (setq org-babel-tangle-use-relative-file-links nil)

  :config
  ;;  Make backtick work as inline `org-vertatim' delimiter.
  (add-to-list 'org-emphasis-alist '("`" org-verbatim verbatim))
  (defun org-set-emph-re (var val)
    "Set variable and compute the emphasis regular expression."
    (set-default-toplevel-value var val)
    (when (and (boundp 'org-emphasis-alist)
	             (boundp 'org-emphasis-regexp-components)
	             org-emphasis-alist org-emphasis-regexp-components)
      (pcase-let*
	        ((`(,pre ,post ,border ,body ,nl) org-emphasis-regexp-components)
	         (body (if (<= nl 0) body
		               (format "%s*?\\(?:\n%s*?\\)\\{0,%d\\}" body body nl)))
	         (template
	          (format (concat "\\([%s]\\|^\\)" ;before markers
			                      "\\(\\([%%s]\\)\\([^%s]\\|[^%s]%s[^%s]\\)\\3\\)"
			                      "\\([%s]\\|$\\)") ;after markers
		                pre border border body border post)))
        (setq org-emph-re (format template "*/_+"))
        ;; Add backtick.
        (setq org-verbatim-re (format template "=~`")))))

  (defun org-do-emphasis-faces (limit)
    "Run through the buffer and emphasize strings."
    (let ((quick-re (format "\\([%s]\\|^\\)\\([~=`*/_+]\\)"
			                      (car org-emphasis-regexp-components))))
      (catch :exit
        (while (re-search-forward quick-re limit t)
	        (let* ((marker (match-string 2))
                 ;; Add backtick.
	               (verbatim? (member marker '("~" "=" "`"))))
	          (when (save-excursion
		                (goto-char (match-beginning 0))
		                (and
		                 ;; Do not match table hlines.
		                 (not (and (equal marker "+")
			                         (org-match-line
			                          "[ \t]*\\(|[-+]+|?\\|\\+[-+]+\\+\\)[ \t]*$")))
		                 ;; Do not match headline stars.  Do not consider
		                 ;; stars of a headline as closing marker for bold
		                 ;; markup either.
		                 (not (and (equal marker "*")
			                         (save-excursion
			                           (forward-char)
			                           (skip-chars-backward "*")
			                           (looking-at-p org-outline-regexp-bol))))
		                 ;; Match full emphasis markup regexp.
		                 (looking-at (if verbatim? org-verbatim-re org-emph-re))
		                 ;; Do not span over paragraph boundaries.
		                 (not (string-match-p org-element-paragraph-separate
					                                (match-string 2)))
		                 ;; Do not span over cells in table rows.
		                 (not (and (save-match-data (org-match-line "[ \t]*|"))
			                         (string-match-p "|" (match-string 4))))))
	            (pcase-let ((`(,_ ,face ,_) (assoc marker org-emphasis-alist))
			                    (m (if org-hide-emphasis-markers 4 2)))
	              (font-lock-prepend-text-property
	               (match-beginning m) (match-end m) 'face face)
	              (when verbatim?
		              (org-remove-flyspell-overlays-in
		               (match-beginning 0) (match-end 0))
                  (when (and (org-fold-core-folding-spec-p 'org-link)
                             (org-fold-core-folding-spec-p 'org-link-description))
                    (org-fold-region (match-beginning 0) (match-end 0) nil 'org-link)
                    (org-fold-region (match-beginning 0) (match-end 0) nil 'org-link-description))
		              (remove-text-properties (match-beginning 2) (match-end 2)
					                                '(display t invisible t intangible t)))
	              (add-text-properties (match-beginning 2) (match-end 2)
				                             '(font-lock-multiline t org-emphasis t))
	              (when (and org-hide-emphasis-markers
			                     (not (org-at-comment-p)))
		              (add-text-properties (match-end 4) (match-beginning 5)
				                               '(invisible t))
		              (add-text-properties (match-beginning 3) (match-end 3)
				                               '(invisible t)))
	              (throw :exit t)))))))))

(use-package org
  :hook
  (org-mode . org-mode-setup)
  (org-mode . individual-visibility-source-blocks)
  (org-babel-after-execute . org-redisplay-inline-images))

(use-package org
  :custom
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t "with `org-appear' it is usable. but need to figure out how to integrate `")
  (org-M-RET-may-split-line nil)
  (org-list-allow-alphabetical t)
  (org-return-follows-link t)
  (org-log-into-drawer t)

  ;; Track todo changes.
  ;; prepend with '/', when leaving this state and the target state does not specify @ or i
  ;; otherwise when entering
  ;; @: note with a timestamp
  ;; !: a timestamp
  ;; /!: a timestamp when *leaving* this state and the target state does not specify @ or i
  (org-todo-keywords '((sequence "TODO(t!)" "DOING(i@/!)" "Q(n@/!)" "|" "DONE(d@)" "POSTPONED(p@)" "DELEGATED(g@)" "CANCELED(c@)")))
  (org-startup-folded t)
  (org-blank-before-new-entry (quote ((heading . nil)
                                      (plain-list-item . nil))))
  (org-edit-src-content-indentation 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-goto-auto-isearch nil)
  (org-hide-block-startup t)

  ;; issue with yasnippet when expanding snippet
  ;; https://www.reddit.com/r/emacs/comments/nj08dz/issues_with_yasnippet_in_emacs_272_lisp_error/
  (org-src-tab-acts-natively nil)
  (org-babel-python-command "py3")
  ;; Try to get width from "#+ATTR_*" keyword and fall back to the original width.
  (org-image-actual-width nil)
  (org-id-link-to-org-use-id 'create-if-interactive)
  (org-src-window-setup 'split-window-below)
  (org-priority-lowest ?D "the default is ?C")
  (org-tags-column -85 nil nil "Customized with use-package org")
  (org-confirm-babel-evaluate nil)
  (org-deadline-warning-days 5)
  (org-imenu-depth 4)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")) "Only use - and +")
  (org-cycle-include-plain-lists 'integrate)
  (org-list-indent-offset 2)
  (org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
  (org-refile-targets '((nil . (:level . 1))
                        (org-agenda-files . (:maxlevel . 1))))
  (org-attach-id-dir "org-attach")

  :custom-face
  (org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3 :width normal :family "Iosevka"))))
  (org-level-2 ((t (:inherit outline-2 :extend nil :height 1.2 :width normal :family "Iosevka"))))
  (org-level-3 ((t (:inherit outline-3 :extend nil :height 1.1 :width normal :weight normal :family "Iosevka"))))
  (org-level-4 ((t (:inherit outline-4 :extend nil :height 1.05 :width normal :weight normal :family "Iosevka"))))
  (org-level-5 ((t (:inherit outline-5 :extend nil :height 1.0 :width normal :weight normal :family "Iosevka"))))
  (org-level-6 ((t (:inherit outline-6 :extend nil :height 1.0 :width normal :weight normal :family "Iosevka"))))
  ;; (org-block ((t (:inherit nil :extend t :background "#282c34")))) ;; the original: "#23272e"
  (org-block-begin-line ((t (:inherit org-block :extend t :foreground "#83898d")))) ;; the original: "#5B6268"
  (org-block ((t (:background "#23272e" :extend t))))
  (org-checkbox-statistics-todo ((t (:inherit org-todo :family "Iosevka"))))
  (org-code ((t (:inherit nil :foreground "#da8548"))))
  (org-verbatim ((t (:foreground "#98be65"))))
  (org-document-title ((t (:foreground "#c678dd" :weight bold :height 1.2 :family "Iosevka"))))
  (org-link ((t (:inherit link :foreground "#51afef" :family "Iosevka"))))
  (org-table ((t (:foreground "#a9a1e1" :slant normal :weight normal :height 180 :width normal :foundry "nil" :family "Sarasa Mono SC Nerd")))))

(use-package org
  :config
  ;; org-agenda
  (defvar  zino/GTD-file "~/Notes/Roam/20220816100518-gtd.org")
  (defvar zino/org-babel-tangle-dir "~/dev/org-babel-tangle/")
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (shell . t)
     (lua . t)
     (dot . t)
     (js . t)
     (python . t)
     (plantuml . t)
     (go . t)))
  ;; Stop repositioning text when cycling visibility
  ;; [reference](https://emacs.stackexchange.com/questions/31276/how-to-prevent-org-mode-from-repositioning-text-in-the-window-when-cycling-visib)
  (remove-hook 'org-cycle-hook
               'org-cycle-optimize-window-after-visibility-change)
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c C-l" . org-store-link)
   ("C-c H-i" . org-insert-link)
   ("<f5>" . org-toggle-inline-images)
   ("<f6>" . (lambda ()
               "Toggle inline image under the current line."
               (interactive)
               (save-excursion
                 (org-display-inline-images
                  t t (beginning-of-line) (end-of-line)))))
   :map org-mode-map
   ([remap org-return-and-maybe-indent] . better-jumper-jump-forward)
   ("C-S-j" . org-return-and-maybe-indent)
   ("C-," . nil)
   ("s-<up>" . org-priority-up)
   ("s-<down>" . org-priority-down)
   :repeat-map
   org-repeat-map
   ("-" . org-ctrl-c-minus)))

(use-package org-mac-image-paste
  :load-path "~/.config/emacs/manually_installed/org-mac-image-paste" ; or wherever you cloned to
  :config (org-mac-image-paste-mode 1)
  :bind (:map org-mode-map ("<f6>" . org-mac-image-paste-refresh-this-node)))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package ultra-scroll-mac
  :if (eq window-system 'mac)
  :load-path "~/.config/emacs/manually_installed/ultra-scroll-mac" ; if you git clone'd
  :init
  (setq scroll-conservatively 101) ; important for jumbo images
  :config
  (ultra-scroll-mac-mode 1))

;; `org-babel' support for evaluating go code
(use-package ob-go)

(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-include-diary t)
  (org-agenda-todo-list-sublevels t)
  (org-agenda-files
   '("~/Notes/Roam/20231025112701-gtd_archive.org" "/Notes/Roam/20220816100518-gtd.org")))

(use-package org-appear)

(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode)
  :custom
  ;; (org-bullets-bullet-list '("⊛" "◎" "◉" "⊚" "○" "●"))
  (org-bullets-bullet-list '("⊛" "◇" "✳" "○" "⊚" "●")))

;; Make sure org-indent face is available
(require 'org-indent)

(use-package org-download
  :after org
  :defer nil
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "~/Pictures/org-images")
  (org-download-heading-lvl nil)
  :bind
  ("C-s-y" . org-download-clipboard))

(defun org-roam-book-template ()
  "Create a Cornell-style book notes template for org-roam node.
Return TEMPLATE as a string."
  (let* ((chapters (read-number "Number of chapters: "))
         (ch 1)
         (template ""))
    (while (<= ch chapters)
      (if (<= ch 9)
          (setq template (concat template (format "* Ch0%d. \n" ch)))
        (setq template (concat template (format "* Ch%d. \n" ch))))
      (setq template (concat template "** Questions [/]\n** Notes\n** Summary\n"))
      (setq ch (1+ ch)))
    (setq template (concat template "* General\n** Questions [/]\n** Notes\n** Summary"))
    template))

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
   (pcase org-journal-file-type
     (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
     (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: fold")
     (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: fold")
     (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: fold"))))

(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
Saves the buffer of the current day's entry and kills the window.
Similar to `org-capture' like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(use-package org-journal
  :preface
  (defun zino/org-journal-new-todo (prefix)
    "Create a new todo entry in `org-journal'."
    (interactive "P")
    (org-journal-new-entry prefix)
    (org-todo))

  (defun zino/org-journal-cycle-after-open-current-journal-file ()
    "Toggle visibility according to buffer's setting."
    (org-cycle t))

  :init
  ;; Change default prefix key, needs to be set before loading
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/Notes/Roam/Journal/")
  (setq org-journal-file-type 'weekly)
  (advice-add 'org-journal-open-current-journal-file :after 'zino/org-journal-cycle-after-open-current-journal-file)

  :custom
  ;; Start on Monday
  (org-journal-start-on-weekday 1)
  (org-journal-file-header 'org-journal-file-header-func)
  ;; YYYY-MM-DD-W{index}
  (org-journal-file-format "%F-W%V.org")
  :bind
  (("C-c j o" . org-journal-open-current-journal-file)
   ("C-c j n" . org-journal-new-entry)
   ("C-c j s" . org-journal-search)
   ("C-c j t" . zino/org-journal-new-todo)
   :map org-journal-mode-map
   ("C-x s-s" . org-journal-save-entry-and-exit))
  :hook
  (org-journal-after-entry-create . org-narrow-to-element))

(use-package org-noter
  :after pdf-tools
  :custom
  (org-noter-auto-save-last-location t)
  (org-noter-closest-tipping-point 1e-05)
  (org-noter-always-create-frame t)
  ;; Hide notes that are not linked to the current document page
  (org-noter-hide-other t)
  (org-noter-notes-window-behavior '(scroll))
  :config
  (defun zino/no-op (&rest args))
  (advice-add 'org-noter--set-notes-scroll :override 'zino/no-op)
  :bind
  (("M-s-." . org-noter)
   :map org-noter-doc-mode-map
   ("i" . org-noter-insert-precise-note)
   ("M-i" . org-noter-insert-note)))

(use-package org-fragtog
  :disabled
  :hook (org-mode . org-fragtog-mode))

(setq zino/roam-dir "~/Notes/Roam"
      zino/anki-file "~/Notes/Roam/20220517104105-anki.org"
      zino/contacts-file "~/Notes/Roam/20220620203106-contacts.org"
      zino/meeting-file (concat zino/roam-dir "/20221115143855-meeting.org"))

(setq org-capture-templates
      `(;; a for "Anki"
        ("a" "Anki")
        ("ab" "Basic card with a front and a back"
         entry
         (file+headline zino/anki-file "Dispatch Shelf")
         "** %<%Y-%m-%d:%H:%M>  %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: %^{Deck|Vocabulary}\n:END:\n*** Front\n%?\n*** Back\n%x\n")
        ("ac" "Cloze"
         entry
         (file+headline zino/anki-file "Dispatch Shelf")
         "** %<%Y-%m-%d:%H:%M>  %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: %^{Deck|Vocabulary}\n:END:\n*** Text\n%?\n*** Back extra\n%x\n")
        ("at" "Temporary Note"
         entry
         (file+headline zino/anki-file "Temporary Notes")
         "** TODO %^{Topic|..}\n%?")

        ;; c for "Contacts"
        ("c" "Contacts")
        ("cf" "Family"
         entry
         (file+headline zino/contacts-file "Family")
         "** %^{Name} \n:PROPERTIES:\n:CREATED: %(format-time-string \"<%Y-%m-%dT%H:%M>\" (current-time))\n:BIRTHDAY: %^{BIRTHDAY}\n:END:\n%?")
        ("cF" "Friends"
         entry
         (file+headline zino/contacts-file "Friends")
         "** %^{Name} \n:PROPERTIES:\n:CREATED: %(format-time-string \"<%Y-%m-%d:%H:%M>\" (current-time))\n:HowDoWeMeet: %^{How do we meet?}\n:END:\n%?")

        ;; g for "Get Things Done"
        ("g" "Get Things Done")
        ("gq" "Questions"
         entry
         (file+regexp zino/GTD-file "\\* Questions \\[[0-9]*/[0-9]*\\]")
         "** TODO %^{What is the QUESTION} %^g\n:PROPERTIES:\n:CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n"
         :immediate-finish nil
         :after-finalize org-fold-hide-drawer-all)
        ("gt" "Tasks"
         entry
         (file+regexp zino/GTD-file "\\* Tasks \\[[0-9]*/[0-9]*\\]")
         "** TODO %^{What is the TASK} %^g\n:PROPERTIES:\n:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n%?"
         :immediate-finish nil
         ;; FIXME: I tried the following to fold the PROPERTY drawer in the
         ;; narrowed org capture buffer. However, when the captured text gets
         ;; inserted, the drawer is unfolded. With `:after-finalize', the
         ;; nullary function is called in the buffer wherein captured text is
         ;; inserted. Currently a relatively expensive
         ;; `org-fold-hide-drawer-all' is called. Optimize it by only folding
         ;; the drawer we just inserted.
         ;; :before-finalize (lambda ()
         ;;                    (org-back-to-heading-or-point-min)
         ;;                    (next-line)
         ;;                    (org-fold-hide-drawer-toggle t))
         ;; NOTE: possible implementation: find the heading we just inserted,
         ;; forward one line, then call `org-fold-hide-drawer-toggle'.
         :after-finalize org-fold-hide-drawer-all)
        ("gl" "Later"
         entry
         (file+regexp zino/GTD-file "\\* Later")
         "** TODO %^{What TO DO later} %^g\n:PROPERTIES:\n:CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n\n"
         :immediate-finish nil
         :after-finalize org-fold-hide-drawer-all)
        ("gr" "Reminders"
         entry
         (file+regexp zino/GTD-file "\\* Reminders \\[[0-9]*/[0-9]*\\]")
         "** TODO %^{What is the REMINDER} %^g\n:PROPERTIES:\n:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n"
         :immediate-finish nil
         :after-finalize org-fold-hide-drawer-all)

        ;; w for "Weekly Plan"
        ("w" "Weekly Plan")
        ("we" "Weekend"
         entry
         (file+function zino/GTD-file (lambda ()
                                        "Locate * Weekly Plan heading. Insert a timestamp of current week if there is none. create a TODO"
                                        (goto-char (point-min))
                                        (unless (re-search-forward "\\* Weekly Plan" (point-max) t)
                                          (goto-char (point-max))
                                          (newline)
                                          (insert "* Weekly Plan\n"))
                                        (unless (re-search-forward (concat "\\*\\* " (format-time-string "%Y-%m") "\\(-[0-9]*\\)?" (format-time-string "-W%V")) (point-max) t)
                                          (newline)
                                          (insert (concat "** " (format-time-string "%Y-%m-%d-W%V\n"))))))
         "*** TODO %^{What to do this weekend} %^g\n:PROPERTIES:\n:CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n\n")

        ;; m for "Meeting"
        ("m" "Meeting")
        ("mr" "Random"
         entry
         (file+headline zino/meeting-file "Participated")
         "** %^{What is it about}  %^g\n:PROPERTIES:\n:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n"
         :immediate-finish nil
         :jump-to-captured t
         :after-finalize org-fold-hide-drawer-all)))

;; Temporary workaround.
;; REVIEW See Wilfred/elisp-refs#35. Remove once fixed upstream.
(defvar read-symbol-positions-list nil)

(use-package rime
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.config/emacs/manually_installed/librime/dist")
  (rime-disable-predicates
   '(rime-predicate-prog-in-code-p
     rime-predicate-auto-english-p
     rime-predicate-punctuation-after-ascii-p
     rime-predicate-punctuation-line-begin-p
     rime-predicate-tex-math-or-command-p
     rime-predicate-org-latex-mode-p
     rime-predicate-current-uppercase-letter-p))
  (rime-cursor "˰")
  (rime-posframe-style 'vertical)
  (rime-show-candidate 'posframe)
  (rime-emacs-module-header-root "/usr/local/Cellar/emacs-plus@29/29.0.60/include/")

  :config
  (define-key rime-active-mode-map (kbd "M-S-j") 'rime-inline-ascii)
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>"
          "<down>" "<prior>" "<next>" "<delete>" "C-`" "C-v" "M-v"))
  :bind
  ("C-s-r" . rime-force-enable))

;; https://emacs-china.org/t/auctex-setup-synctex-with-pdf-tools-not-working/11257
;; (use-package auctex
;;   :ensure t
;;   :no-require t
;;   :commands (TeX-latex-mode)
;;   :config
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq-default TeX-master nil)

;;   ;; Use `xetex' engine for better TeX compilation for Chinese.
;;   ;; `TeX-engine-alist', `TeX-engine-in-engine-alist'
;;   (setq-default TeX-engine 'xetex)
;;   (with-eval-after-load 'tex-mode
;;     ;; "latexmk -shell-escape -bibtex -xelatex -g -f %f"
;;     (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf"))
;;     (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
;;     (setq TeX-command "xelatex"))

;;   (setq-default LaTeX-command  "latex -shell-escape --synctex=1")

;;   (setq TeX-show-compilation t)

;;   ;; view generated PDF with `pdf-tools'. (this is built-in now.)
;;   (unless (assoc "PDF Tools" TeX-view-program-list-builtin)
;;     (add-to-list 'TeX-view-program-list-builtin '("PDF Tools" TeX-pdf-tools-sync-view)))
;;   (unless (equalp "PDF Tools" (car (cdr (assoc 'output-pdf TeX-view-program-selection))))
;;     (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))

;;   ;; [ SyncTeX ] -- Sync (forward and inverse search) PDF with TeX/LaTeX.
;;   (setq TeX-source-correlate-mode t)
;;   (setq TeX-source-correlate-method '((dvi . source-specials) (pdf . synctex))) ; default
;;   ;; [C-c C-g] switch between LaTeX source code and PDF positions.
;;   (setq TeX-source-correlate-start-server t)
;;   (TeX-source-correlate-mode t)
;;   ;; update PDF buffers after successful LaTeX runs.
;;   (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook #'TeX-revert-document-buffer))

;; (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))

;;; arch
;; epdfinfo runs poppler in server mode, receives request from emacs
;; poppler render pdf into PNGs on the go and return them to emacs
;; poppler can provide rich information about the pdf and also the
;; ability to modify
;; Download `pdf-tools' from melpa handles dependencies for us
(use-package pdf-tools
  :init
  (pdf-tools-install)
  :preface
  (defun zino/pdf-tools-toggle-mouse-1-use ()
    "Toggle `pdf-view-selection-style' between word and glyph."
    (interactive)
    (if (eq pdf-view-selection-style 'glyph)
        (setq pdf-view-selection-style 'word)
      (setq pdf-view-selection-style 'glyph)))

  :bind
  (:map pdf-view-mode-map
        ("n" . pdf-view-next-line-or-next-page)
        ("p" . pdf-view-previous-line-or-previous-page)
        ("," . pdf-view-previous-page)
        ("." . pdf-view-next-page)
        ("C-<down-mouse-1>" . zino/pdf-tools-toggle-mouse-1-use)
        ("[" . shrink-window-horizontally)
        ("]" . enlarge-window-horizontally)
        ("s" . isearch-forward)
        ("f" . image-forward-hscroll)
        ("b" . image-backward-hscroll)
        ("1" . pdf-annot-add-highlight-markup-annotation)
        ("2" . pdf-annot-add-underline-markup-annotation)
        ("3" . pdf-annot-add-squiggly-markup-annotation)
        ("4" . pdf-annot-delete)
        ("5" . pdf-annnt-add-text-annotation))
  :custom
  ;; If scrolling cause noticeable delays, try setting it to nil
  (pdf-view-use-scaling t)
  (pdf-view-continuous t)
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  ;; Select by word by default and use `zino/pdf-tools-toggle-mouse-1-use' to toggle
  (pdf-view-selection-style 'word)
  (pdf-view-use-imagemagick t)
  ;; :config
  ;; (define-key pdf-links-minor-mode-map [remap pdf-links-isearch-link] 'image-forward-hscroll)
  )

(use-package pdf-links-minor-mode
  :after pdf-tools
  :ensure nil
  :bind
  (:map pdf-links-minor-mode-map
        ("f" . image-forward-hscroll)
        ("S" . pdf-links-isearch-link)))

(use-package doc-toc
  :ensure nil
  :load-path "~/.config/emacs/manually_installed/doc-tools-toc")

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :bind
  (:map nov-mode-map
        ("n" . next-line)
        ("p" . previous-line)
        ("f" . forward-char)
        ("b" . backward-char)))

;;(defun my-nov-font-setup ()
;;  (variable-pitch-mode)
;;  (display-line-numbers-mode -1)
;;  (face-remap-add-relative 'variable-pitch :family "ETBembo" :height 1.1))
;;(add-hook 'nov-mode-hook 'my-nov-font-setup)

;;(require 'justify-kp)
;;(setq nov-text-width t)
;;
;;(defun my-nov-window-configuration-change-hook ()
;;  (my-nov-post-html-render-hook)
;;  (remove-hook 'window-configuration-change-hook
;;               'my-nov-window-configuration-change-hook
;;               t))
;;
;;(defun my-nov-post-html-render-hook ()
;;  (if (get-buffer-window)
;;      (let ((max-width (pj-line-width))
;;            buffer-read-only)
;;        (save-excursion
;;          (goto-char (point-min))
;;          (while (not (eobp))
;;            (when (not (looking-at "^[[:space:]]*$"))
;;              (goto-char (line-end-position))
;;              (when (> (shr-pixel-column) max-width)
;;                (goto-char (line-beginning-position))
;;                (pj-justify)))
;;            (forward-line 1))))
;;    (add-hook 'window-configuration-change-hook
;;              'my-nov-window-configuration-change-hook
;;              nil t)))

;;(add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)

;;(defvar message-filter-regexp-list '("^Starting new Ispell process \\[.+\\] \\.\\.\\.$"
;;                                     "^Ispell process killed$"
;;                                     ;;"^down-mouse"
;;                                     "^down-mouse-.*"
;;                                     "down-mouse-1"
;;                                     "^double-down-mouse.*"
;;                                     "double-down-mouse-1"
;;                                     ;;" . *mouse.*")
;;                                     )
;;  "filter formatted message string to remove noisy messages")

;;(defadvice message (around message-filter-by-regexp activate)
;;  (if (not (ad-get-args 0))
;;      ad-do-it
;;    (let ((formatted-string (apply 'format (ad-get-args 0))))
;;      (if (and (stringp formatted-string)
;;               (some (lambda (re) (string-match re formatted-string)) message-filter-regexp-list))
;;          (save-excursion
;;            (set-buffer "*Messages*")
;;            (goto-char (point-max))
;;            (insert formatted-string "\n"))
;;        (progn
;;          (ad-set-args 0 `("%s" ,formatted-string))
;;          ad-do-it)))))

;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "ETBembo" :height 250 :weight regular))))
;;  '(fixed-pitch ((t ( :family "Fira Code" :height 250)))))

(set-fontset-font
 t 'symbol
 (font-spec
  :family "Apple Color Emoji"
  :size 16
  :weight 'normal
  :width 'normal
  :slant 'normal))

(use-package beacon
  :config
  (beacon-mode -1)
  :custom
  (beacon-blink-delay 0.05)
  (beacon-color "#a8dadc")
  (beacon-size 30)
  (beacon-blink-when-buffer-changes t)
  (beacon-blink-when-window-changes nil)
  (beacon-blink-when-window-scrolls nil)
  (beacon-blink-when-focused t)
  (beacon-blink-when-point-moves-vertically 50)
  :custom-face
  (beacon-fallback-background ((t (:background "#a8dadc")))))

(setq custom-safe-themes t)

(setq display-line-numbers-width 0)
(setq display-line-numbers-width-start 0)

(setq apropos-sort-by-scores t)

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame)
  (setq aw-ignored-buffers '("*Calc Trail*" " *LV*"))
  :bind
  ("M-a" . ace-swap-window)
  ("M-o" . ace-window)
  ("s-q" . ace-delete-window)
  :custom-face
  (aw-leading-char-face ((t (:foreground "red" :weight bold :height 1.0))))
  :custom
  (aw-char-position 'top-left))

(setq next-screen-context-lines 2)

(blink-cursor-mode)
(setq blink-cursor-blinks 0)

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file (concat user-emacs-directory "bookmarks"))
  ;; Column width of bookmark name in `bookmark-menu' buffer
  (bookmark-bmenu-file-column 80))

;; Registers
(use-package register
  :ensure nil
  :custom
  (register-preview-delay 0.1)
  :bind
  ("s-x" . copy-to-register)
  ("s-i" . insert-register))

(defun zino/increment-number-decimal (&optional arg)
  "Increment the number forward from point by ARG."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789" (point-min))
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun zino/decrement-number-decimal (&optional arg)
  "Decrement the number forward from point by ARG."
  (interactive "p*")
  (zino/increment-number-decimal (if arg (- arg) -1)))

(use-package fzf
  :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "rg --no-heading -nH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15)
  ;; (global-set-key (kbd "C-c f") #'fzf)
  ;; (global-set-key (kbd "C-c d") #'fzf-directory)
  )

(repeat-mode)

(pixel-scroll-mode 1)

;; autosave file-visiting buffer but not non-file-visiting e.g. *scratch*
(setq-default auto-save-default t)
(setq auto-save-timeout 15)
(setq auto-save-interval 100)
(setq-default auto-save-no-message t)

(setq
 auto-save-file-name-transforms
 `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.config/emacs/autosave/\\2" t)))

(setq
 ;; turn on backup functionality
 make-backup-files t
 ;; also backup files that are version controlled
 vc-make-backup-files t
 ;; backup by renaming will make the existing file the backup, and so all old
 ;; links to the original file now links to the backup and not the edited file
 backup-by-copying t
 ;; control the naming of different versions ourselves
 version-control nil
 ;; don't ask to delete old version
 delete-old-versions t
 kept-new-versions 20
 kept-old-versions 20)

;; by default emacs create backup on first buffer save since the file is visited
;; C-u to save backup in second save (test above)
;; C-u C-u to immediately save into backup
(setq backup-directory-alist nil)

(defun make-backup-file-name (file)
  (let ((dirname (concat "~/.config/emacs/backup/"
                         (format-time-string "%Y-%m-%d/"))))
    (unless (file-exists-p dirname)
      (make-directory dirname t))
    ;; new backups in the same minute will overwrite the old backups. Append
    ;; second infomation if needed
    (concat dirname (concat (replace-regexp-in-string "/" "!" (buffer-file-name)) (format-time-string "T%H:%M~")))))

(defun force-backup-of-buffer ()
  "Utilize the standard backup system to make a backup everytime a buffer is saved."
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook 'force-backup-of-buffer)

(use-package super-save)

(defun find-file--auto-create-dir (filename &optional wildcards)
  "Create parent directory during visiting file if necessary .
Do not prompt me to create parent directory"
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(advice-add 'find-file :before 'find-file--auto-create-dir)

(delete-selection-mode 1)

(use-package consult
  :preface
  (defun zino/consult-imenu-thing-at-point (prompt items)
    "Select from imenu ITEMS given PROMPT string with the symbol at point as
initial input."
    (consult-imenu--deduplicate items)
    (consult-imenu--jump
     (consult--read
      (or items (user-error "Imenu is empty"))
      :state
      (let ((preview (consult--jump-preview)))
        (lambda (action cand)
          ;; Only preview simple menu items which are markers,
          ;; in order to avoid any bad side effects.
          (funcall preview action (and (markerp (cdr cand)) (cdr cand)))))
      :narrow
      (when-let (narrow (consult-imenu--narrow))
        (list :predicate
              (lambda (cand)
                (eq (get-text-property 0 'consult--type (car cand)) consult--narrow))
              :keys narrow))
      :group (consult-imenu--group)
      :prompt prompt
      :require-match t
      :category 'imenu
      :lookup #'consult--lookup-cons
      :history 'consult-imenu--history
      :add-history (thing-at-point 'symbol)
      :initial (thing-at-point 'symbol)
      :sort nil)))

  :custom
  ;; Maually and immidiate
  (consult-preview-key "M-.")
  (consult-narrow-key "<")
  :bind
  ("M-i" . consult-imenu)
  ("C-s-i" . (lambda ()
               (interactive)
               (zino/consult-imenu-thing-at-point
                "Go to item: "
                (consult--slow-operation "Building Imenu..."
                  (consult-imenu--items)))))
  ("M-s-i" . consult-imenu-multi)
  ("s-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-M-y" . consult-yank-from-kill-ring)
  ("s-5" . consult-kmacro)
  ([remap bookmark-jump] . consult-bookmark)

  :config
  (advice-add 'consult-bookmark :around (lambda (oldfun &rest args)
                                          (apply oldfun args)
                                          (recenter))))

;;; Freeze emacs after each `save-buffer' and out of nowhere when I am just scrolling. It must've done some
;;; heavy background work.
;; (use-package consult-projectile
;;   :bind
;;   ([remap projectile-file-file] . consult-project-find-file)
;;   ([remap projectile-switch-project] . consult-projectile-switch-project))

(use-package consult-todo
  :load-path "~/.config/emacs/manually_installed/consult-todo/")

(use-package hl-line
  :hook
  (dired-mode . hl-line-mode)
  :config
  (set-face-attribute 'hl-line nil :inherit nil :background "#21242b") ;;"#2e3b49")
  (set-face-attribute 'region nil :inherit nil :distant-foreground "#959ba5" :background "#42444a"));; "dark slate gray"));;"#113d69"));;"#2e4a54")) ;;"#406389")) ;; "#42444a")) ;; #4f5b66

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package go-mode
  :bind
  (:map go-mode-map
        ([remap beginning-of-defun] . go-goto-function)))

(use-package go-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  :hook
  (go-ts-mode . tree-sitter-hl-mode)
  :custom
  (go-ts-mode-indent-offset 2))

(use-package tramp
  :custom
  (tramp-default-proxies-alist nil)
  (tramp-verbose 3)
  (tramp-auto-save-directory "~/tmp/tramp/")
  (tramp-chunksize 2000))

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(defun my/tree-sitter-compile-grammar (destination &optional path)
  "Compile grammar at PATH, and place the resulting shared library in DESTINATION."
  (interactive "fWhere should we put the shared library? \nfWhat tree-sitter grammar are we compiling? \n")
  (make-directory destination 'parents)

  (let* ((default-directory
          (expand-file-name "src/" (or path default-directory)))
         (parser-name
          (thread-last (expand-file-name "grammar.json" default-directory)
                       (json-read-file)p
                       (alist-get 'name)))
         (emacs-module-url
          "https://raw.githubusercontent.com/casouri/tree-sitter-module/master/emacs-module.h")
         (tree-sitter-lang-in-url
          "https://raw.githubusercontent.com/casouri/tree-sitter-module/master/tree-sitter-lang.in")
         (needs-cpp-compiler nil))
    (message "Compiling grammar at %s" path)

    (url-copy-file emacs-module-url "emacs-module.h" :ok-if-already-exists)
    (url-copy-file tree-sitter-lang-in-url "tree-sitter-lang.in" :ok-if-already-exists)

    (with-temp-buffer
      (unless
          (zerop
           (apply #'call-process
                  (if (file-exists-p "scanner.cc") "c++" "cc") nil t nil
                  "parser.c" "-I." "--shared" "-o"
                  (expand-file-name
                   (format "libtree-sitter-%s%s" parser-name module-file-suffix)
                   destination)
                  (cond ((file-exists-p "scanner.c") '("scanner.c"))
                        ((file-exists-p "scanner.cc") '("scanner.cc")))))
        (user-error
         "Unable to compile grammar, please file a bug report\n%s"
         (buffer-string))))
    (message "Completed compilation")))

;; (use-package tree-sitter-rust
;;   :straight
;;   (:type git
;;          :host github
;;          :repo "tree-sitter/tree-sitter-rust")
;;   :init
;;   (setq treesit-extra-load-path '("~/.config/emacs/ts-grammars/")))

(setq treesit-extra-load-path '("~/.config/emacs/ts-grammars/"))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode)
  (rust-ts-mode . cargo-minor-mode))

(use-package rust-mode
  ;; When (treesit-ready-p 'rust) is t, `rust-ts-mode' registers itself for file
  ;; name pattern "\\.rs\\'". Currently use `rust-mode' in rust files.
  ;; :after rust-ts-mode
  :custom
  (rust-indent-offset 4))

(use-package rust-ts-mode
  ;; :after tree-sitter-rust
  :config
  (setq auto-mode-alist (delete '("\\.rs\\'" . rust-ts-mode) auto-mode-alist)))

(use-package rustic
  :load-path "~/.config/emacs/manually_installed/rustic/"
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-default-test-arguments nil)
  ;; :config
  ;; NOTE: These hooks are added in `rustic.el'. Remove them if intend to use
  ;; `flymake'.
  ;; (remove-hook 'rustic-mode-hook 'flymake-mode-off)
  ;; (remove-hook 'rustic-mode-hook 'flycheck-mode)
  :bind
  (
   :map rust-mode-map ("C-c C-p" . zino/rustic-popup)
   :map rust-ts-mode-map ("C-c C-p" . zino/rustic-popup)
   :map rustic-compilation-mode-map ("p" . previous-error-no-select))
  :config
  ;; Reverse the action of making `rustic-mode' default for rust files in
  ;; `rustic.el'.
  ;; (setq auto-mode-alist (delete '("\\.rs\\'" . rustic-mode) auto-mode-alist))
  ;; (setf (alist-get "\\.rs\\'" auto-mode-alist nil nil 'string=) 'rust-mode)
  (defun zino/rustic-popup (&optional args)
    "Setup popup.
If directory is not in a rust project call `read-directory-name'."
    (interactive "P")
    (rustic--inheritenv
     (setq rustic--popup-rust-src-name buffer-file-name)
     (let ((func (lambda ()
                   (let ((buf (get-buffer-create rustic-popup-buffer-name))
                         (win (split-window-below))
                         (inhibit-read-only t))
                     (rustic-popup-insert-contents buf)
                     (set-window-buffer win buf)
                     (select-window win)
                     ;; (fit-window-to-buffer nil nil nil nil nil t)
                     ;; (set-window-text-height win (+ (window-height) 1))
                     ))))
       (if args
           (let ((dir (read-directory-name "Rust project:")))
             (let ((default-directory dir))
               (funcall func)))
         (funcall func))))))

(use-package cmake-mode)

(use-package js
  :config
  (setq js-indent-level 2)
  :hook
  (js-mode . (lambda ()
               (setq format-all-formatters '(("JavaScript" deno))))))

(use-package lsp-mode
  :disabled
  :init
  (setq lsp-clangd-binary-path (executable-find "clangd"))
  (setq lsp-clients-lua-language-server-bin (executable-find "lua-language-server"))
  ;; Use `eglot' for now
  :hook
  (;; bug with company and template completion, switch to eglot
   (python-mode . lsp)
   (lua-mode . lsp)
   (c-mode . lsp)
   (c++-mode . lsp)
   (cmake-mode . lsp)
   (go-mode . lsp)
   (rust-mode . lsp)
   (jsonc-mode . lsp)
   (sh-mode . lsp)
   (html-mode . lsp)
   (css-mode . lsp)
   (js-mode . lsp)
   (nginx-mode . lsp))
  :config
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-signature-auto-activate t
        lsp-signature-render-documentation t)
  :bind
  (:map lsp-mode-map
        ("C-c C-l" . lsp-treemacs-symbols)
        ("C-c C-d" . xref-find-definitions)
        ("C-c d" . xref-find-definitions-other-window)
        ("C-c r" . lsp-rename)
        ("C-c C-e" . flycheck-list-errors)
        ("C-c C-r" . lsp-find-references))
  :custom
  (lsp-eldoc-enable-hover t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-signature-render-documentation t)
  (lsp-diagnostics-provider :flycheck)
  (lsp-clients-lua-lsp-server-install-dir "/usr/local/bin/lua-language-server")
  (lsp-elm-only-update-diagnostics-on-save t)
  (lsp-go-library-directories '("~/go/src/goroot" "/usr"))
  (warning-suppress-types '((lsp-mode)))
  :custom
  ;; what to use when checking on-save. "check" is default.
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.5)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "never")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-clients-clangd-args '("--header-insertion=iwyu"))  ;; [use]
  (lsp-intelephense-multi-root nil)  ;; reset lsp session in lsp mode
  ;; (setq lsp--session nil)
  )

(use-package lsp-ui
  :after lsp-mode
  :commands
  lsp-ui-mode
  :custom-face
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  (lsp-ui-doc-background ((t (:inherit tooltip :background "#2e3138"))))
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-border "#3d4554");;"#51afef") ;;"white")  ;; (face-foreground 'default)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-max-height 120)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-webkit-max-width-px 800)

  ;; lsp-ui-sideline
  (lsp-ui-sideline-mode t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-lens-enable nil)
  (lsp-headerline-breadcrumb-enable nil)

  :hook
  (lsp-mode . lsp-ui-mode)
  :config
  (define-key lsp-ui-imenu-mode-map (kbd "n") 'next-line)
  (define-key lsp-ui-imenu-mode-map (kbd "p") 'previous-line))

(use-package markdown-mode
  :custom-face
  ;; Remove the default udnerline indicating line breaks
  (markdown-line-break-face ((t (:underline nil :inherit font-lock-constant-face))))
  :config
  ;; \` matches beginning of string
  ;; \' matches end of string
  ;; ^ matches beginning of (buffer || string || line)
  ;; $ matches end of (buffer || string || line)
  (add-to-list 'auto-mode-alist '("\\`README\\'" . markdown-mode)))

(use-package eglot
  ;;; Performance tweaking
  :init
  ;; Disable logging.
  (fset #'jsonrpc--log-event #'ignore)
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly)
    ;; Customize the order various doc strings are displayed.
    (setq eldoc-documentation-functions '(flymake-eldoc-function
                                          eglot-signature-eldoc-function
                                          eglot-hover-eldoc-function)))

  :hook
  ;; Don't integrate flycheck's error messages and eglot's documentation for
  ;; now. Use `flycheck-display-error-messages'.
  ;; (eglot-managed-mode . mp-flycheck-prefer-eldoc)
  (eglot-managed-mode . (lambda ()
                          (flymake-mode -1)))

  :init
  ;; eglot use this variable to determine if `company-mode' ignores case
  (setq completion-ignore-case t)

  :config
  ;; NOTE: Change existing `eglot-server-programs'
  ;; (setf (alist-get 'yaml-mode eglot-server-programs) "my/special/path/to/yaml/server")
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--clang-tidy" "--header-insertion=iwyu")))
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd" "--clang-tidy" "--header-insertion=iwyu")))
  (add-to-list 'eglot-server-programs '(cmake-mode . ("cmake-language-server")))
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer" "-v")))
  (add-to-list 'eglot-server-programs '(beancount-mode . ("beancount-language-server")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls")))
  ;; (add-to-list 'eglot-server-programs '(conf-toml-mode . ("taplo")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t))
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . xref-find-definitions)
        ("C-c d" . xref-find-definitions-other-window)
        ("C-c r"   . eglot-rename)
        ("C-c C-r" . xref-find-references)
        ("C-c H-i" . eglot-find-implementation)
        ("C-c C-a" . eglot-code-actions))
  :hook
  (rust-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (lua-mode . eglot-ensure)
  (cmake-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  (css-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (beancount-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  :custom-face
  ;; (eglot-highlight-symbol-face ((t (:foreground "#DFDFDF" :background "#34536c" :weight bold))))
  (eglot-highlight-symbol-face ((t (:weight bold))))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  ;; NOTE: Important. The default is nil, and will cause `xref-find-definitions'
  ;; to fail in rust crates. (TODO: find out why it failed.)
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t))

(use-package eglot
  :config
  ;; https://github.com/joaotavora/eglot/issues/98
  (defun zino/project-try-cargo-toml (dir)
    "Try to locate a Rust project above DIR."
    (let ((found (locate-dominating-file dir "Cargo.toml")))
      (if (stringp found) `(transient . ,found) nil)))

  ;; Try rust projects before version-control (vc) projects
  (add-hook 'project-find-functions 'zino/project-try-cargo-toml nil nil))

;; How to translate LSP configuration examples into Eglot’s format:
;;
;; Usually LSP servers will say something like
;;
;; rust-analyzer.procMacro.attributes.enable (default: true)
;;
;; Translate that into a JSON LSP configuration, you get
;;
;; {
;;   "rust-analyzer": {
;;     "procMacro": {
;;       "attributes": {
;;         "enable": true
;;       }
;;     }
;;   }
;; }
;;
;; In general the key at the root level must be the LSP server’s name,
;; so that one configuration can cover multiple servers. If the LSP
;; server’s documentation omitted the name part, remember to add it
;; yourself.
;;
;; Translate that into plist format that Eglot uses, you get
;;
;; (:rust-analyzer (:procMacro (:attributes (:enable t))))
;;
;; Keys translate to keywords (with the same spelling and case),
;; dictionaries translate to plists, arrays translate to vectors, true
;; translates to t, false translates to a special keyword :json-false,
;; null translates to nil, empty directory {} translates to eglot-{}.

;; Define a setup function that runs in the mode hook.
;; (defun setup-rust ()
;;   "Setup for ‘rust-mode’."
;;   ;; Configuration taken from rust-analyzer’s manual:
;;   ;; https://rust-analyzer.github.io/manual.html#configuration
;;   (setq-local eglot-workspace-configuration
;;               ;; Setting the workspace configuration for every
;;               ;; rust-mode buffer, you can also set it with dir-local
;;               ;; variables, should you want different configuration
;;               ;; per project/directory.
;;               '(:rust-analyzer
;;                 ( :procMacro ( :attributes (:enable t)
;;                                :enable t)
;;                   :autoImport (:enable t)
;;                   :cargo (:buildScripts (:enable t))
;;                   :diagnostics (:disabled ["unresolved-proc-macro"
;;                                            "unresolved-macro-call"])))))

;; Define a custom eglot LSP server for rust-analyzer because it
;; expects initializationOptions done a bit differently (see below).
;; (defclass eglot-rust-analyzer (eglot-lsp-server) ()
;;   :documentation "A custom class for rust-analyzer.")

;; Rust-analyzer requires the workspaceConfiguration sent as
;; initializationOptions at startup time. See
;; https://github.com/joaotavora/eglot/discussions/845 and
;; rust-analyzer’s manual page.
;; (cl-defmethod eglot-initialization-options ((server eglot-rust-analyzer))
;;   eglot-workspace-configuration)

;; Use our custom ‘eglot-rust-analyzer’ for ‘rust-mode’.
;; (add-to-list 'eglot-server-programs
;;              '(rust-mode . (eglot-rust-analyzer "rust-analyzer")))

;; Define a setup function that runs in the mode hook.
;; (defun setup-rust ()
;;   "Setup for ‘rust-mode’."
;;   ;; Configuration taken from rust-analyzer’s manual:
;;   ;; https://rust-analyzer.github.io/manual.html#configuration
;;   (interactive)
;;   (setq-local eglot-workspace-configuration
;;               ;; Setting the workspace configuration for every
;;               ;; rust-mode buffer, you can also set it with dir-local
;;               ;; variables, should you want different configuration
;;               ;; per project/directory.
;;               '(:rust-analyzer
;;                 (:autoImport (:enable t)))))

;; Run our setup function in ‘rust-mode-hook’.
;; (add-hook 'rust-mode-hook #'setup-rust)

(use-package eglot-x
  :ensure nil
  :load-path "~/.config/emacs/manually_installed/eglot-x")

(use-package yasnippet
  :hook
  (after-init . yas-global-mode))

(use-package yasnippet-snippets)

(use-package flymake-cursor
  :load-path "~/.config/emacs/manually_installed/emacs-flymake-cursor")

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("C-c [" . flymake-goto-prev-error)
        ("C-c ]" . flymake-goto-next-error)
        ("C-c C-e" .
         (lambda ()
           "Open the window listing errors and switch to it."
           (interactive)
           (flymake-show-buffer-diagnostics)
           (pop-to-buffer "*Flymake diagnostics*"))))
  :config
  (eval-after-load 'flymake '(require 'flymake-cursor))
  (defun flymake--diagnostics-buffer-name ()
    (format "*Flymake diagnostics*"))
  :hook
  (prog-mode . (lambda ()
                 (flymake-mode -1))))

(use-package flycheck
  :preface
  (defun mp-flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
                  (format "%s: %s"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize "I" 'face 'flycheck-error-list-info))
                              ('error (propertize "E" 'face 'flycheck-error-list-error))
                              ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                              (_ level)))
                          (flycheck-error-message err))
                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err))
                  :face 'font-lock-doc-face))
       flycheck-errors)))

  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions 'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))

  (defun zino/flycheck-next-error-advice (oldfun &rest args)
    (apply oldfun args)
    (recenter))

  :hook
  (prog-mode . flycheck-mode)
  ;; MAYBE: use directory variables to configure per project
  (c++-mode . (lambda ()
                (setq flycheck-clang-language-standard "c++17")))

  ;; NOTE: this will slow emacs down
  ;; :config
  ;; (push 'rustic-clippy flycheck-checkers)
  :custom
  (flycheck-display-errors-delay 0.6)
  (flycheck-indication-mode 'left-fringe)
  (flycheck-go-golint-executable "go-staticcheck")
  :bind
  (:map flycheck-mode-map
        ("C-c [" . flycheck-previous-error)
        ("C-c ]" . flycheck-next-error)
        ("s-." . flycheck-display-error-at-point)
        ("C-c C-e" . (lambda ()
                       "Open the window listing errors and switch to it."
                       (interactive)
                       (flycheck-list-errors)
                       (pop-to-buffer "*Flycheck errors*"))))
  :config
  (advice-add 'flycheck-next-error :around 'zino/flycheck-next-error-advice))

(use-package flycheck-rust
  :hook
  (flycheck-mode . flycheck-rust-setup))

;; (use-package flycheck-eglot
;;   :config
;;   (global-flycheck-eglot-mode -1))

(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

(defvar flycheck-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "[" #'flycheck-previous-error)
    (define-key map "]" #'flycheck-next-error)
    map)
  "Repeat map for flycheck.")

(defvar flymake-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "[" #'flymake-goto-prev-error)
    (define-key map "]" #'flymake-goto-next-error)
    map)
  "Repeat map for flymake.")

(defvar isearch-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'isearch-repeat-forward)
    (define-key map "r" #'isearch-repeat-backward)
    map)
  "Repeat map for isearch.")

(defvar winner-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") 'winner-undo)
    (define-key map (kbd "<right>") 'winner-redo)
    map)
  "Repeat map for `winner-mode'.")

(defvar org-remark-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "[") 'org-remark-view-prev)
    (define-key map (kbd "]") 'org-remark-view-next)
    map)
  "Repeat map for `org-remark'.")

(repeatize 'isearch-repeat-map)
(repeatize 'flycheck-repeat-map)
(repeatize 'winner-repeat-map)
(repeatize 'flymake-repeat-map)
(repeatize 'org-remark-repeat-map)

;; Show matching parenthesis
(use-package mic-paren
  :custom-face
  (paren-face-match ((t (:foreground "#7bb6e2" :background "#2c3946" :weight ultra-bold))))
  ;; :config
  ;; (paren-activate)
  :custom
  (paren-display-message 'always))

;; Boxes comment
;; Site: https://boxes.thomasjensen.com/
(defun boxes-create ()
  "Convert a region into box comments specified by `-d' option."
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end) "boxes -d c-cmt2" nil 1 nil))

(defun boxes-remove ()
  "Convert a region of box comments into plain code."
  (interactive)
  (shell-command-on-region (region-beginning)
                           (region-end) "boxes -r -d c-cmt2" nil 1 nil))

(use-package button-lock)

(use-package anki-editor
  ;; check https://github.com/louietan/anki-editor/issues/76
  :hook
  (org-capture-after-finalize . zino/anki-editor-reset-cloze-num)
  (org-capture . anki-editor-mode)
  :config
  (defun zino/anki-editor-reset-cloze-num ()
    "Reset cloze number of current card to 1."
    (interactive)
    (setq anki-editor-cur-cloze-num 1))

  (defun zino/anki-editor-cloze-region-auto-incr ()
    "Cloze active region or word under corsor without hint.
  Automatically increase cloze number"
    (interactive)
    (anki-editor-cloze-dwim anki-editor-cur-cloze-num "")
    (sp-forward-sexp)
    (setq anki-editor-cur-cloze-num (1+ anki-editor-cur-cloze-num)))

  (defun zino/anki-editor-cloze-region-not-incr ()
    "Cloze active region or word under corsor without hint.
  Do not increase cloze number"
    (interactive)
    (anki-editor-cloze-dwim anki-editor-cur-cloze-num "")
    (sp-forward-sexp))

  (defun zino/anki-editor-push-tree ()
    "Push cards of the org subtree under cursor to Anki."
    (interactive)
    (anki-editor-push-notes '(4))
    (zino/anki-editor-reset-cloze-num))

  (defun zino/anki-editor-push-buffer ()
    "Push cards of this buffer."
    (interactive)
    (anki-editor-push-notes)
    (zino/anki-editor-reset-cloze-num))

  :config
  ;; initialize cloze states
  (zino/anki-editor-reset-cloze-num)
  (setq anki-editor-create-decks t
        anki-editor-org-tags-as-anki-tags t)
  :bind
  ("C-c m i" . anki-editor-insert-note)
  ("C-c m r" . zino/anki-editor-reset-cloze-num)
  ("C-c m c" . zino/anki-editor-cloze-region-auto-incr)
  ("C-c m p" . zino/anki-editor-push-tree)
  ("C-c m b" . zino/anki-editor-push-buffer))

(use-package ankiorg
  :straight (:host github :repo "orgtre/ankiorg"))

(use-package font-lock
  :ensure nil
  :custom-face
  (font-lock-comment-face ((t (:foreground "#83898d")))) ;; the original: ;; "#5B6268"
  ;; (font-lock-doc-face ((t (:family "Iosevka" :foreground "#7cb8bb"))));; "#7F9F7F"))));;"#9FC59F")))) ;;"#8CA276")))) ;; the original: "#83898d"
  (font-lock-doc-face ((t (:family "Fira Code" :foreground "#83898d" :inherit font-lock-comment-face)))))

(use-package winner
  :hook
  (after-init . winner-mode))

;; lua mode
(use-package lua-mode
  :bind
  (:map lua-mode-map
        ([remap beginning-of-defun] . lua-beginning-of-proc)
        ([remap end-of-defun] . lua-end-of-proc))
  :custom
  (lua-indent-level 4))

(add-to-list 'load-path "~/.config/emacs/manually_installed/lua-mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . perl-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(add-to-list 'auto-mode-alist '(".*Makefile" . makefile-gmake-mode))

(use-package cc-mode
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.inl$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))
  :bind
  (:map c-mode-base-map
        ([remap c-toggle-comment-style] . copy-line)
        ([remap beginning-of-defun] . c-beginning-of-defun)
        ([remap end-of-defun] . c-end-of-defun)
        ("C-M-a" . sp-backward-up-sexp)
        ("C-M-e" . sp-up-sexp)
        ("M-a" . ace-swap-window)
        ("M-e" . forward-word)))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil)
  (dabbrev-limit 6))

(use-package hippie-exp
  :bind
  ([remap dabbrev-expand] . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-line
     try-expand-all-abbrevs
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-list
     try-expand-dabbrev-all-buffers
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))

(use-package corfu
  :custom
  (corfu-cycle t)                      ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                       ;; Enable auto completion
  (corfu-separator ?\s)                ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)  ;; Quit at separator
  (corfu-quit-no-match t)              ;; Quit if there is no match
  (corfu-preview-current 'insert)      ;; Disable current candidate preview
  (corfu-preselect 'first)             ;; Preselect the prompt
  (corfu-on-exact-match 'insert)       ;; Configure handling of exact matches
  (corfu-scroll-margin 5)              ;; Use scroll margin
  (corfu-auto-delay 0)                 ;; Auto-suggestion delay
  (corfu-auto-prefix 2)                ;; Auto-suggestion minimum prefix
  (corfu-popupinfo-delay '(0.2 . 0))   ;; Initial and subsequent delay
  (tab-always-indent 'complete)
  :custom
  (corfu-count 10)
  (corfu-popupinfo-max-height 150)
  (corfu-popupinfo-min-height 80)
  (corfu-popupinfo-min-width 80)
  (corfu-popupinfo-max-width 150)

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("<tab>"      . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("<ret>" . corfu-insert))

  ;; This is recommended since Dabbrev can be used globally (M-/s ).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :custom-face
  (corfu-bar ((t (:background "#a8a8a8" :weight bold))))
  (corfu-border ((t (:weight bold :width extra-expanded))))
  (corfu-current ((t (:background "#2c3946" :foreground "#bbc2cf")))))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

;; Add extensions for `completion-at-point-functions'
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  ;; :bind (("C-c p p" . completion-at-point) ;; capf
  ;;        ("C-c p t" . complete-tag)        ;; etags
  ;;        ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ("C-c p k" . cape-keyword)
  ;;        ("C-c p s" . cape-symbol)
  ;;        ("C-c p a" . cape-abbrev)
  ;;        ("C-c p l" . cape-line)
  ;;        ("C-c p w" . cape-dict)
  ;;        ("C-c p \\" . cape-tex)
  ;;        ("C-c p _" . cape-tex)
  ;;        ("C-c p ^" . cape-tex)
  ;;        ("C-c p &" . cape-sgml)
  ;;        ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :custom
  (cape-dabbrev-min-length 6))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(flex orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  :hook
  (sh-mode . (lambda ()
               (setq-local completion-styles '(orderless basic partial-completion))))
  (lua-mode . (lambda ()
                (setq-local completion-styles '(orderless basic partial-completion))))
  (emacs-lisp-mode . (lambda ()
                       "Prevent sluggish in `emacs-lisp-mode'."
                       (setq-local completion-at-point-functions (remove 'cape-dabbrev completion-at-point-functions)
                                   completion-styles '(orderless basic partial-completion)
                                   corfu-auto-delay 0.2)))

  ;; flex completion style is too sluggish with `eval-expression' so use
  ;; orderless by default. This works coz `eval-expression-minibuffer-setup-hook'
  ;; is run after `minibuffer-mode-hook'. NOTE: `minibuffer-setup-hook' is run
  ;; after entry to minibuffer, and hence is not suitable to set completion
  ;; styles as it cannot differentiate `eval-expression' from `M-x'.
  (eval-expression-minibuffer-setup . (lambda ()
                                        (setq-local completion-styles '(orderless partial-completion basic))))
  :config
  (add-hook 'minibuffer-mode-hook (defun completion-styles-for-minibuffer ()
                                    "Enable autocompletion on files."
                                    (add-to-list 'completion-at-point-functions 'cape-file)
                                    (setq-local completion-styles '(flex orderless partial-completion basic)))))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter))

(use-package lsp-bridge
  :disabled
  :load-path "~/.config/emacs/manually_installed/lsp-bridge"
  ;; Use `eglot' for now
  :hook
  ;; statically typed
  (c-mode . lsp-bridge-mode)
  (c++-mode . lsp-bridge-mode)
  ;; dynamically typed
  (python-mode . lsp-bridge-mode)
  (lua-mode . lsp-bridge-mode)
  (go-mode . lsp-bridge-mode)
  (sh-mode . lsp-bridge-mode)
  ;; DSL
  (cmake-mode . lsp-bridge-mode)
  (css-mode . lsp-bridge-mode)
  (html-mode . lsp-bridge-mode)
  :bind
  (:map lsp-bridge-mode-map
        ("C-c C-d" . lsp-bridge-find-define)
        ("C-c C-e" . lsp-bridge-list-diagnostics)
        ("C-o" . lsp-bridge-return-from-def)
        ("C-c C-o" . lsp-bridge-lookup-documentation)
        ("C-c C-v" . acm-doc-scroll-down)
        ("C-c C-M-v" . acm-doc-scroll-up))
  :custom
  (acm-doc-frame-max-lines 100)
  (lsp-bridge-completion-popup-predicates
   '(lsp-bridge-not-only-blank-before-cursor
     lsp-bridge-not-match-hide-characters
     lsp-bridge-not-match-stop-commands
     lsp-bridge-not-in-string
     lsp-bridge-not-in-comment
     lsp-bridge-not-follow-complete
     lsp-bridge-multiple-cursors-disable
     lsp-bridge-not-complete-manually)))

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; git-modes
(use-package git-modes)

;; Customize `rg-menu' to pass additional flags to rg:
;; https://emacs.stackexchange.com/questions/74040/using-rg-ripgrep-passing-files-without-match-option-flag
(use-package rg
  :ensure-system-package
  (rg . ripgrep)
  :custom
  (rg-executable (executable-find "rg"))
  (rg-custom-type-aliases '(("sls" . "*.sls")))
  (rg-hide-command nil)
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*rg\\*" . (nil . ((body-function . select-window))))))

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package nginx-mode)

(use-package comint
  :ensure nil
  :bind
  (:map comint-mode-map
        ([remap kill-whole-line] . comint-kill-input))
  :custom
  (comint-prompt-read-only t))

;; Set limit for prompt opening large files higher, 100 M
(setq large-file-warning-threshold 100000000)

(use-package format-all
  :hook
  (prog-mode . format-all-ensure-formatter)
  (prog-mode . format-all-mode)
  :custom
  (format-all-show-errors 'never)
  (format-all-default-formatters
   '(("Assembly" asmfmt)
     ("ATS" atsfmt)
     ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex)
     ("C" clang-format)
     ("C#" csharpier)
     ("C++" clang-format)
     ("Cabal Config" cabal-fmt)
     ("Clojure" zprint)
     ("CMake" cmake-format)
     ("Crystal" crystal)
     ("CSS" prettier)
     ("Cuda" clang-format)
     ("D" dfmt)
     ("Dart" dart-format)
     ("Dhall" dhall)
     ("Dockerfile" dockfmt)
     ("Elixir" mix-format)
     ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp)
     ("Erlang" efmt)
     ("F#" fantomas)
     ("Fish" fish-indent)
     ("Fortran Free Form" fprettify)
     ("GLSL" clang-format)
     ("Go" gofmt)
     ("GraphQL" prettier)
     ("Haskell" brittany)
     ("HTML" html-tidy)
     ("HTML+EEX" mix-format)
     ("HTML+ERB" erb-format)
     ("Java" clang-format)
     ("JavaScript" deno)
     ("JSON" prettier)
     ("JSON5" prettier)
     ("Jsonnet" jsonnetfmt)
     ("JSX" prettier)
     ("Kotlin" ktlint)
     ("LaTeX" latexindent)
     ("Less" prettier)
     ("Literate Haskell" brittany)
     ("Lua" lua-fmt)
     ("Markdown" prettier)
     ("Nix" nixpkgs-fmt)
     ("Objective-C" clang-format)
     ("OCaml" ocp-indent)
     ("Perl" perltidy)
     ("PHP" prettier)
     ("Protocol Buffer" clang-format)
     ("PureScript" purty)
     ("Python"
      (black "--skip-string-normalization"))
     ("R" styler)
     ("Reason" bsrefmt)
     ("ReScript" rescript)
     ("Ruby" rufo)
     ("Rust"
      (rustfmt "--edition" "2021" "--config" "tab_spaces=4"))
     ("Scala" scalafmt)
     ("SCSS" prettier)
     ("Shell"
      (shfmt "-i" "2"))
     ("Solidity" prettier)
     ("SQL" sqlformat)
     ("Svelte" prettier)
     ("Swift" swiftformat)
     ("Terraform" terraform-fmt)
     ("TOML" prettier)
     ("TSX" prettier)
     ("TypeScript" prettier)
     ("V" v-fmt)
     ("Verilog" istyle-verilog)
     ("Vue" prettier)
     ("XML" html-tidy)
     ("YAML" prettier)
     ("Zig" zig)
     ("_Angular" prettier)
     ("_Caddyfile" caddy-fmt)
     ("_Flow" prettier)
     ("_Gleam" gleam)
     ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt)
     ("_Snakemake" snakefmt)))
  :config
  (defun format-all-ensure-formatter ()
    "Ensure current buffer has a formatter, using default if not."
    (interactive)
    (let ((language (format-all--language-id-buffer)))
      (unless (format-all--get-chain language)
        (cond ((not language)
               (message "No formatter for this language"))
              ((not (gethash language format-all--language-table))
               (message "No formatter for %s" language))
              (t
               (let ((default (format-all--get-default-chain language)))
                 (cond ((not default)
                        (message "No default formatter for %s" language))
                       (t
                        ;; (message "Using default formatter%s"
                        ;;          (with-temp-buffer
                        ;;            (dolist (formatter default (buffer-string))
                        ;;              (insert (format " %S" formatter)))))
                        (format-all--set-chain language default))))))))))

(use-package better-jumper
  :preface
  (defun zino/better-jumper-advice (oldfun &rest args)
    "Call OLDFUN with ARGS, then set a jump point with `better-jumper-set-jump'."
    (let ((old-pos (point)))
      (better-jumper-set-jump old-pos)
      (apply oldfun args)
      ;; (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point)))) 1)
      ;;   (better-jumper-set-jump old-pos))
      ))

  :init
  ;; https://stackoverflow.com/questions/4512075/how-to-use-ctrl-i-for-an-emacs-shortcut-without-breaking-tabs
  (keyboard-translate ?\C-i ?\H-i)
  :custom
  (better-jumper-context 'window)
  :bind
  (("C-o" . better-jumper-jump-backward)
   ("H-i" . better-jumper-jump-forward)
   :map org-mode-map
   ;; Not better jumpper related, but also use 'H-i' keybinding
   ("C-c C-x H-i" . org-clock-in))

  :config
  (better-jumper-mode 1)

  (advice-add 'xref-find-definitions :around 'zino/better-jumper-advice)
  (advice-add 'zino/switch-other-buffer :around 'zino/better-jumper-advice)
  (advice-add 'helm-imenu :around 'zino/better-jumper-advice)
  (advice-add 'widget-button-press :around 'zino/better-jumper-advice)
  (advice-add 'org-open-at-point :around 'zino/better-jumper-advice)
  (advice-add 'beginning-of-buffer :around 'zino/better-jumper-advice)
  (advice-add 'end-of-buffer :around 'zino/better-jumper-advice)
  (advice-add 'c-beginning-of-defun :around 'zino/better-jumper-advice)
  (advice-add 'c-end-of-defun :around 'zino/better-jumper-advice)
  (advice-add 'lua-beginning-of-proc :around 'zino/better-jumper-advice)
  (advice-add 'lua-end-of-proc :around 'zino/better-jumper-advice)
  (advice-add 'org-open-at-point :around 'zino/better-jumper-advice)
  (advice-add 'counsel-bookmark :around 'zino/better-jumper-advice)
  (advice-add 'mark-whole-buffer :around 'zino/better-jumper-advice)
  (advice-add 'beginning-of-defun :around 'zino/better-jumper-advice)
  (advice-add 'end-of-defun :around 'zino/better-jumper-advice)
  (advice-add 'org-roam-node-find :around 'zino/better-jumper-advice)
  (advice-add 'zino/find-user-init-file :around 'zino/better-jumper-advice)
  (advice-add 'avy-goto-char-timer :around 'zino/better-jumper-advice)
  (advice-add 'helm-maybe-exit-minibuffer :around 'zino/better-jumper-advice)
  (advice-add 'consult-imenu :around 'zino/better-jumper-advice)
  (advice-add 'consult-line :around 'zino/better-jumper-advice)
  (advice-add 'symbols-outline-move-depth-up :around 'zino/better-jumper-advice))

(use-package org-remark
  :config
  (defun zino/org-remark-notes-file-name-function ()
    "Customize notes file."
    (let ((notes-dir (concat "~/Documents/org-remark" (file-name-directory (buffer-file-name)))))
      (make-directory notes-dir t)
      (concat notes-dir (file-name-base (buffer-file-name)) "-notes.org")))

  ;;; `org-remark' builtin function redefinition
  (defun org-remark-open (point &optional view-only)
    "Open remark note buffer if there is notes from `point' to the beginning
  of the line and automatically unfold the note headline.
  This is a modified version of the function in `org-remark'."
    (interactive "d\nP")
    (when-let ((id
                ;; #+begin_modified
                (let ((point-beginning-of-line (line-beginning-position))
                      remark-id)
                  (while (>= point point-beginning-of-line)
                    (if (get-char-property point 'org-remark-id)
                        (progn
                          (setq remark-id (get-char-property point
                                                             'org-remark-id))
                          (setq point (1- point-beginning-of-line)))
                      (setq point (1- point))))
                  remark-id))
               ;; #+end_modified
               (ibuf (org-remark-notes-buffer-get-or-create))
               (cbuf (current-buffer)))
      (pop-to-buffer ibuf org-remark-notes-display-buffer-action)
      (widen)
      (when-let (p (org-find-property org-remark-prop-id id))
        ;; Somehow recenter is needed when a highlight is deleted and move to a
        ;; previous highlight.  Otherwise, the cursor is too low to show the
        ;; entire entry.  It looks like there is no entry.
        (goto-char p)(org-narrow-to-subtree)(org-end-of-meta-data t)(recenter)
        ;; #+begin_modified
        (if (version< emacs-version "29")
            ;; `org-fold-show-entry' is available after Emacs 29
            (org-show-hidden-entry)
          (org-fold-show-entry))
        ;; #+end_modified
        )
      ;; Avoid error when buffer-action is set to display a new frame
      (when-let ((view-only view-only)
                 (window (get-buffer-window cbuf)))
        (select-window window))))

  ;;; `org-remark' builtin function redefinition
  (defun org-remark-delete (point)
    "Delete the nearest highlight and marginal notes from POINT to the beginning of line.

  This function will prompt for confirmation if there is any notes
  present in the marginal notes buffer.  When the marginal notes
  buffer is not displayed in the current frame, it will be
  temporarily displayed together with the prompt for the user to
  see the notes.

  If there is no notes, this function will not prompt for
  confirmation and will remove the highlight and deletes the entry
  in the marginal notes buffer.

  This command is identical with passing a universal argument to
  `org-remark-remove'."
    (interactive "d")
    (let ((bol (line-beginning-position)))
      (save-excursion
        (while (>= point bol)
          (if (get-char-property point 'org-remark-id)
              (org-remark-remove point :delete)
            (setq point (1- point)))))))

  (defun zino/org-remark-mark-and-open ()
    "Helper function to mark region and open notes buffer.
  I find myself often do this workflow."
    (interactive)
    (org-remark-mark (region-beginning) (region-end))
    (org-remark-open (point)))

  (defun zino/org-remark-open-advice (point &optional view-only)
    "Display source code edit buffer in `other-window' cause a `side-window'
  cannot be splitted."
    (setq-local org-src-window-setup 'other-window))

  :config
  (advice-add 'org-remark-open :after 'zino/org-remark-open-advice))

(use-package org-remark
  :init
  ;; Turn on `org-remark' highlights on startup
  (org-remark-global-tracking-mode +1)

  :bind
  ;; (keyboard-translate ?\C-m ?\H-m)
  (("C-x C-n m" . org-remark-mark)
   ("C-x C-n o" . org-remark-open)
   ("C-x C-n v" . org-remark-view)
   ("C-x C-n ]" . org-remark-view-next)
   ("C-x C-n [" . org-remark-view-prev)
   ("C-x C-n r" . org-remark-remove)
   ("C-x C-n d" . org-remark-delete)
   ("C-x C-n y" . org-remark-mark-yellow)
   ("C-x C-n l" . org-remark-mark-red-line)
   ("C-x C-n e" . zino/org-remark-mark-and-open)
   ("C-x C-n t" . org-remark-toggle))
  :custom
  (org-remark-notes-display-buffer-action
   '((display-buffer-in-side-window)
     (side . left)
     (slot . 1)
     (window-width . 45)))
  (org-remark-notes-file-name 'zino/org-remark-notes-file-name-function)

  :custom-face
  (org-remark-highlighter ((t (:background "#023047" :underline nil)))))

(use-package org-bulletproof
  :disabled
  :config
  (global-org-bulletproof-mode -1)
  :custom
  (org-bulletproof-default-ordered-bullet "1."))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;; one tab for one workspace
(use-package tab-bar
  :config
  (tab-bar-mode +1)
  ;; it is not common to mis-type this
  (global-unset-key (kbd "C-<tab>"))
  :bind
  ("M-<tab>" . tab-next)
  ("M-S-<tab>" . tab-previous)
  ("C-s-<tab>" . tab-bar-select-tab-by-name)
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-show nil)
  (tab-width 2))

(use-package tab-bookmark
  :ensure nil
  :load-path "~/.config/emacs/manually_installed/tab-bookmark")

(use-package python
  :config
  (with-eval-after-load 'python-mode
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-detect-failure-warning nil))
  (add-to-list 'auto-mode-alist '("\\.py[iw]?\\'" . python-mode))
  :custom
  (python-eldoc-function-timeout 0.2))

;; Google's gn meta build system
(use-package gn-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.gn\\'" . gn-mode)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package isearch
  :preface
  (defun zino/isearch-region-or-forward ()
    "Isearch thing in region if region is active, otherwise perform normal isearch."
    (interactive)
    (if (use-region-p)
        (isearch-forward-thing-at-point)
      (isearch-forward)))

  :ensure nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format "  (%s/%s)")
  (isearch-allow-scroll 'unlimited)

  :bind
  ("C-s-s" . zino/isearch-region-or-forward)
  :config
  (put 'zino/isearch-region-or-forward 'repeat-map isearch-repeat-map))

(defun zino/inhibit-buffer-messages ()
  "Set `inhibit-message' buffer-locally."
  (setq-local inhibit-message t))

(use-package nasm-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode)))

(use-package json-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json\\'" . jsonc-mode)))

(use-package elfeed
  :custom
  (elfeed-feeds
   '("https://www.reddit.com/r/emacs.rss"
     "https://news.ycombinator.com/rss"
     "https://fasterthanli.me/index.xml")))

(use-package cc-mode
  :ensure nil
  :config
  (defconst zino/cc-style
    '("cc-mode"
      (c-offsets-alist . ((innamespace . [0])))))
  (c-add-style "cc-mode" zino/cc-style)
  :bind
  (:map c-mode-base-map
        ([remap c-indent-line-or-region] . indent-for-tab-command)))

(use-package org-present
  :init
  (defun zino/org-present-on-start ()
    (visual-fill-column-mode 1)
    (setq-local visual-fill-column-width 120)
    (setq-local org-hide-emphasis-markers t)
    ;; (face-remap-add-relative 'org-block-begin-line '(:background "#282c34"))
    ;; (face-remap-add-relative 'org-block-end-line '(:background "#282c34"))
    )

  (defun zino/org-present-on-quit ()
    (visual-fill-column-mode -1)
    (setq-local org-hide-emphasis-markers nil)
    ;; (face-remap-add-relative 'org-block-begin-line '(:foreground "#83898d" :background "#23272e"))
    ;; (face-remap-add-relative 'org-block-end-line '(:foreground "#83898d" :background "#23272e"))
    )

  (defun zino/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines.
    (org-overview)
    ;; Unfold the current entry.
    (org-fold-show-entry)
    ;; Show only direct subheadings of the slide but don't expand them
    (org-fold-show-children))
  :hook
  (org-present-mode . zino/org-present-on-start)
  (org-present-mode-quit . zino/org-present-on-quit)
  :config
  (add-hook 'org-present-after-navigate-functions 'zino/org-present-prepare-slide)
  :custom
  (visual-fill-column-center-text t))

(use-package visual-fill-column)

(use-package god-mode
  :bind
  ("S-<escape>" . god-mode-all)
  ("C-c g" . god-local-mode)
  (:map god-local-mode-map
        ("C-<f14>" . scroll-lock-mode)))

;; deal with terminal escape characters correctly in compilation buffer
(use-package ansi-color
  :ensure nil
  :config
  (defun zino/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max) t)))
  (add-to-list 'auto-mode-alist '("\\.log\\(\\.[0-9]*-[0-9]*-[0-9]*\\)?" . zino/ansi-colorize-buffer))
  :hook
  (compilation-filter . zino/ansi-colorize-buffer))

(use-package avy
  :bind
  ("s-l" . avy-goto-line)
  ("C-s-l" . avy-goto-end-of-line)
  ("M-j" . avy-goto-char-timer)
  :custom
  (avy-background nil))

(use-package avy-zap
  :config
  (defun zino/zap-up-to-char (arg char &optional interactive)
    "Customized `zap-up-to-char' to support reading a double quote from the
mibuffer. The only difference is this function uses `read-char' instead of
`read-char-from-minibuffer'. The rest of the doc is copied from the original
function.

Kill up to, but not including ARGth occurrence of CHAR.  When run
interactively, the argument INTERACTIVE is non-nil.  Case is ignored if
`case-fold-search' is non-nil in the current buffer.  Goes backward if ARG is
negative; error if CHAR not found.  Ignores CHAR at point.  If called
interactively, do a case sensitive search if CHAR is an upper-case character."
    (interactive "p\ncZap up to char: ")
    (let ((direction (if (>= arg 0) 1 -1))
          (case-fold-search (if (and interactive (char-uppercase-p char))
                                nil
                              case-fold-search)))
      (kill-region (point)
		               (progn
		                 (forward-char direction)
		                 (unwind-protect
		                     (search-forward (char-to-string char) nil nil arg)
		                   (backward-char direction))
		                 (point)))))
  :bind
  ("M-z" . avy-zap-up-to-char-dwim)
  ("M-Z" . avy-zap-to-char-dwim)
  ("C-z" . zino/zap-up-to-char))

(use-package crux
  :preface
  (defun zino/find-user-init-file (other-window)
    "Open `user-init-file' in the current or other window based on OTHER-WINDOW."
    (interactive "P")
    (if current-prefix-arg
        (find-file-other-window user-init-file)
      (find-file user-init-file)))

  :bind
  ("C-s-o" . crux-smart-open-line-above)
  ("s-d" . crux-duplicate-current-line-or-region)
  ("C-<backspace>" . crux-kill-line-backwards)
  ;; ("s-r" . crux-recentf-find-file)
  ("C-c I" . zino/find-user-init-file)
  ("C-c D" . crux-delete-file-and-buffer))

(use-package pulsar
  :config
  (pulsar-global-mode -1)
  :hook
  (next-error-hook . pulsar-pulse-line)
  :custom
  (pulsar-delay 0.05)
  (pulsar-face 'beacon-fallback-background)
  (pulsar-iterations 3)
  ;; :config
  ;; (setq pulsar-pulse-functions (remove 'recenter-top-bottom pulsar-pulse-functions))
  )

(use-package xref
  :ensure nil
  :bind
  ("C-c M-d" . xref-find-definitions-other-frame)
  ("C-c C-r" . xref-find-references)
  :custom
  (xref-history-storage 'xref-window-local-history)
  (xref-search-program 'ripgrep)
  (xref-auto-jump-to-first-xref nil)
  :custom-face
  (xref-file-header ((t (:inherit orderless-match-face-0))))
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*xref\\*"
     display-buffer-in-side-window
     (side . bottom)
     (slot . 0)
     (window-height . 20)
     (window-parameters
      (no-delete-other-windows . t))))
  :hook
  (xref-after-jump . beacon-blink)
  (xref-after-return . beacon-blink)
  (xref-after-return . recenter)
  ;; :config
  ;; (remove-hook 'xref-after-jump-hook 'beacon-blink)
  ;; (remove-hook 'xref-after-return-hook 'beacon-blink)
  )

(use-package eshell
  :disabled
  :ensure nil
  :hook
  (eshell-mode . (lambda ()
                   (setq-local corfu-auto nil)))

  :config
  (with-eval-after-load 'esh-mode
    (define-key eshell-mode-map (kbd "C-k") 'eshell-kill-input))
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color"))))

(use-package esh-autosuggest
  :hook
  (eshell-mode . esh-autosuggest-mode))

(use-package eshell-toggle
  :disabled
  :after eshell
  :bind
  ("s-e" . eshell-toggle))

;; (use-package eat
;;   :load-path "~/.config/emacs/manually_installed/emacs-eat/")

(use-package vterm
  :bind
  (:map vterm-mode-map
        ("s-<return>" . vterm-send-return)
        ("C-<return>" . vterm-send-return)
        ("C-s-t" . vterm-copy-mode))
  (:map vterm-copy-mode-map
        ("q" . vterm-copy-mode)
        ("C-s-t" . vterm-copy-mode))
  :custom
  (vterm-timer-delay 0.01))

(use-package vterm-toggle
  :bind
  ("s-e" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  ;; (add-to-list 'display-buffer-alist
  ;;              '((lambda (buffer-or-name _)
  ;;                  (let ((buffer (get-buffer buffer-or-name)))
  ;;                    (with-current-buffer buffer
  ;;                      (or (equal major-mode 'vterm-mode)
  ;;                          (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
  ;;                (display-buffer-reuse-window display-buffer-in-side-window)
  ;;                (side . bottom)
  ;;                ;;(dedicated . t) ;dedicated is supported in emacs27
  ;;                (reusable-frames . visible)
  ;;                (window-height . 0.3)))
  :custom
  (vterm-toggle-scope 'project))

(use-package fish-mode)

(use-package fish-completion
  ;; `fish-completion-mode' is unbearably slow
  :disabled
  :load-path "~/.config/emacs/manually_installed/emacs-fish-completion"
  :hook
  (eshell-mode . fish-completion-mode))

(use-package nyan-mode
  :config
  (nyan-mode -1)
  :custom
  (nyan-cat-face-number 1)
  )

(use-package screenshot
  :load-path "~/.config/emacs/manually_installed/screenshot/"
  :custom
  (screenshot-line-numbers-p t))

(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode"))

(use-package breadcrumb
  :load-path "~/.config/emacs/manually_installed/breadcrumb/"
  :custom
  (breadcrumb-project-max-length 0)
  (breadcrumb-imenu-max-length 80))

;; This will mess up `org-ellipsis', don't use it for now.
(use-package pp-c-l
  :disabled
  :ensure nil
  :load-path "~/.config/emacs/manually_installed"
  ;; :custom
  ;; (pp^L-^L-string "                                            ")
  :config
  (pretty-control-l-mode))

(use-package prism)

(use-package bookmark+
  :load-path "~/.config/emacs/manually_installed/bookmark-plus"
  :custom
  (bmkp-last-as-first-bookmark-file "~/.config/emacs/bookmarks"))

(use-package tla-mode
  :load-path "~/.config/emacs/manually_installed/tla-mode")

(use-package tla-tools
  :load-path "~/.config/emacs/manually_installed/tla-tools")

(use-package tree-sitter-langs)

(use-package tree-sitter
  ;; :after tree-sitter-langs
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :custom
  (tree-sitter-hl-use-font-lock-keywords t)
  :custom-face
  (tree-sitter-hl-face:function.call ((t (:inherit (link font-lock-function-name-face) :underline nil :weight normal))))
  (tree-sitter-hl-face:property ((t (:inherit font-lock-constant-face)))))

(add-to-list 'tree-sitter-major-mode-language-alist '(go-ts-mode . go))
(add-to-list 'tree-sitter-major-mode-language-alist '(rust-ts-mode . go))

;; (use-package treesit-auto
;;   :config
;;   (global-treesit-auto-mode))

;; (use-package tree-sitter-rust
;;   :straight
;;   (:host github
;;          :repo "tree-sitter/tree-sitter-rust"
;;          :post-build
;;          (my/tree-sitter-compile-grammar
;;           (expand-file-name "ts-grammars" user-emacs-directory)))
;;   :init
;;   (setq treesit-extra-load-path '("~/.config/emacs/ts-grammars/"))
;;   (defun my/tree-sitter-compile-grammar (destination &optional path)
;;     "Compile grammar at PATH, and place the resulting shared library in DESTINATION."
;;     (interactive "fWhere should we put the shared library? \nfWhat tree-sitter grammar are we compiling? \n")
;;     (make-directory destination 'parents)

;;     (let* ((default-directory
;;             (expand-file-name "src/" (or path default-directory)))
;;            (parser-name
;;             (thread-last (expand-file-name "grammar.json" default-directory)
;;                          (json-read-file)p
;;                          (alist-get 'name)))
;;            (emacs-module-url
;;             "https://raw.githubusercontent.com/casouri/tree-sitter-module/master/emacs-module.h")
;;            (tree-sitter-lang-in-url
;;             "https://raw.githubusercontent.com/casouri/tree-sitter-module/master/tree-sitter-lang.in")
;;            (needs-cpp-compiler nil))
;;       (message "Compiling grammar at %s" path)

;;       (url-copy-file emacs-module-url "emacs-module.h" :ok-if-already-exists)
;;       (url-copy-file tree-sitter-lang-in-url "tree-sitter-lang.in" :ok-if-already-exists)

;;       (with-temp-buffer
;;         (unless
;;             (zerop
;;              (apply #'call-process
;;                     (if (file-exists-p "scanner.cc") "c++" "cc") nil t nil
;;                     "parser.c" "-I." "--shared" "-o"
;;                     (expand-file-name
;;                      (format "libtree-sitter-%s%s" parser-name module-file-suffix)
;;                      destination)
;;                     (cond ((file-exists-p "scanner.c") '("scanner.c"))
;;                           ((file-exists-p "scanner.cc") '("scanner.cc")))))
;;           (user-error
;;            "Unable to compile grammar, please file a bug report\n%s"
;;            (buffer-string))))
;;       (message "Completed compilation"))))

(use-package separedit
  :bind
  ("C-c C-'" . separedit))

(use-package modus-themes)

(use-package org-ql)

(use-package beancount
  :ensure nil
  :load-path "~/.config/emacs/manually_installed/beancount-mode"
  :init
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
  :commands beancount-mode
  :bind
  (:map beancount-mode-map
        ("<tab>" . indent-for-tab-command)))

(use-package plantuml-mode
  :custom
  (plantuml-executable-path "plantuml")
  (plantuml-default-exec-mode 'executable)
  (org-plantuml-jar-path (car (directory-files-recursively "/usr/local/Cellar/plantuml" ".*\.jar")))
  (plantuml-indent-level 2)
  :init
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode)))

(use-package gdb-mi
  :ensure nil
  :bind
  (:map gdb-frames-mode-map
        ("n" . next-line)
        ("p" . previous-line))
  :custom
  (gdb-window-configuratin-directory user-emacs-directory)
  (gdb-default-window-configuration-file "gdb_window_configurations")
  (gdb-many-windows t)
  (gdb-restore-window-configuration-after-quit 'if-gdb-many-windows)

  :bind
  ("C-c ; b" . gud-break)
  ("C-c ; d" . gud-remove)
  ("C-c ; f" . gud-finish)
  ("C-c ; i" . gud-stepi)
  ("C-c ; j" . gud-jump)
  ("C-c ; l" . gud-refresh)
  ("C-c ; n" . gud-next)
  ("C-c ; p" . gud-print)
  ("C-c ; r" . gud-continue)
  ("C-c ; s" . gud-step)
  ("C-c ; t" . gud-tbreak)
  ("C-c ; u" . gud-until)
  ("C-c ; w" . gud-watch)
  ("C-c ; <" . gud-up)
  ("C-c ; >" . gud-down)
  (:repeat-map
   gud-gdb-repeat-map
   ("i" . gud-stepi)
   ("s" . gud-step)
   ("c" . gud-continue)
   ("f" . gud-finish)
   ("l" . gud-refresh)
   ("n" . gud-next)))

(use-package realgud
  :custom
  (realgud-safe-mode nil)
  (realgud-window-split-orientation 'horizontal))

(use-package realgud-lldb)

(use-package prog-face-refine)

(use-package peek
  :straight (:type git :host sourcehut :repo "meow_king/peek")
  :bind
  (("C-c p d" . peek-xref-definition)
   ("C-c p p" . peek-overlay-dwim)
   :map peek-mode-keymap
   ("M-s-n" . peek-next-line)
   ("M-s-p" . peek-prev-line)
   ("M-n" . nil)
   ("M-p" . nil))

  :custom
  (peek-method 'overlay)
  :custom-face
  (peek-overlay-content-face ((t (:extend t :background "#23272e")))))

(use-package hyperbole
  :disabled)

(use-package ef-themes
  :load-path "~/.config/emacs/manually_installed/ef-themes/")

;; (use-package org-valign)

(use-package indent-bars
  :load-path "manually_installed/indent-bars/"
  :hook
  (hack-local-variables . (lambda ()
                            ;; Read `indent-bars-space-override' from
                            ;; `.dir-locals.el' first.
                            (when (derived-mode-p 'rust-mode)
                              (indent-bars-mode))))
  (lua-mode . indent-bars-mode)
  (json-mode . indent-bars-mode)
  (js-mode . indent-bars-mode)
  (c++-mode . indent-bars-mode)
  (c-mode . indent-bars-mode)
  (go-mode . indent-bars-mode)
  (go-ts-mode . indent-bars-mode)
  (python-ts-mode . indent-bars-mode)
  (python-mode . indent-bars-mode)
  (sh-mode . indent-bars-mode)
  :custom
  (indent-bars-color-by-depth
   '(:palette
     (outline-1 outline-2 outline-3 outline-4 outline-5 outline-6 outline-7)
     :blend 1))
  (indent-bars-pattern ". . . . ")
  (indent-bars-width-frac 0.2)
  (indent-bars-treesit-support nil)
  :hook
  ;; HACK: deal with uncorrectly displayed indent-bars in org mode source block.
  (org-mode . (lambda ()
                (face-remap-add-relative 'indent-bars-1 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-2 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-3 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-4 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-5 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-6 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-7 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-8 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-9 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-10 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-11 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-12 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-13 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-14 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-15 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-16 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-17 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-18 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-19 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-20 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-21 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-22 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-23 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-24 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-25 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-26 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-27 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-28 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-29 '(:foreground nil))
                (face-remap-add-relative 'indent-bars-30 '(:foreground nil)))))

(use-package embark
  :config
  (eval-when-compile
    (defmacro zino/embark-ace-action (fn)
      `(defun ,(intern (concat "zino/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))
  (define-key embark-general-map (kbd "o") (zino/embark-ace-action projectile-find-file))
  (define-key embark-general-map (kbd "n") (zino/embark-ace-action org-roam-node-find))
  (define-key embark-buffer-map (kbd "o") (zino/embark-ace-action switch-to-buffer))
  :bind
  ("s-o" . embark-act))

(use-package embark-consult)

(use-package visual-regexp
  :bind
  ([remap query-replace-regexp] . vr/query-replace))

(use-package posframe-plus
  :load-path "~/.config/emacs/manually_installed/posframe-plus")

(use-package treesitter-context
  :after posframe-plus
  :load-path "~/.config/emacs/manually_installed/treesitter-context.el"
  :custom
  (treesitter-context-hide-frame-after-move t)
  (treesitter-context-idle-time 0.1))

;; Mandatory, as the dictionary misbehaves!
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*" display-buffer-in-side-window
               (side . left)
               (window-width . 50)))

(use-package symbols-outline
  :custom
  (symbols-outline-window-position 'left)
  (symbols-outline-no-other-window nil)
  (symbols-outline-window-width 42)
  :config
  (symbols-outline-follow-mode)
  (setq symbols-outline-fetch-fn 'symbols-outline-lsp-fetch)
  :bind
  ("C-M-i" . symbols-outline-show)
  :hook
  (sh-mode . (lambda ()
               (symbols-outline-follow-mode -1))))

(use-package zoom-window
  :bind
  ("C-s-z" . zoom-window-zoom)
  :custom
  (zoom-window-mode-line-color "black"))

(use-package simple
  :ensure nil
  :bind
  ("C-/" . undo-only)
  ("M-_" . undo-redo)
  :custom
  (undo-no-redo t))

(use-package undo-hl
  ;; Too distracting
  :disabled
  :load-path "~/.config/emacs/manually_installed/undo-hl/"
  :hook
  (prog-mode . undo-hl-mode)
  :custom
  (undo-hl-flash-duration 0.02)
  (undo-hl-mininum-edit-size 5)
  (undo-hl-undo-commands
   '(undo undo-only undo-redo undo-fu-only-undo undo-fu-only-redo evil-undo evil-redo undo-tree-undo undo-tree-redo)))

(use-package vundo
  :bind
  ("C-x u" . vundo))

;; Customizing colors used in diff mode
;; (defun custom-diff-colors ()
;;   "update the colors for diff faces"
;;   (set-face-attribute
;;    'diff-added nil :foreground "green")
;;   (set-face-attribute
;;    'diff-removed nil :foreground "red")
;;   (set-face-attribute
;;    'diff-changed nil :foreground "purple"))
;; (eval-after-load "diff-mode" '(custom-diff-colors))

;;; File manipulation
(defun zino/insert-buffer-file-name (full-path)
  "Insert file name of the buffer at point.
Insert full path if prefix argument `FULL-PATH' is sent."
  (interactive "P")
  (let ((f (buffer-file-name)))
    (if (not full-path)
        (setq f (file-name-nondirectory f)))
    (insert f)))

(use-package rebox2
  :load-path "~/.config/emacs/manually_installed/"
  :bind
  ("C-:" . rebox-cycle))

(use-package sh-script
  :ensure nil
  :custom
  (sh-basic-offset 2))

(use-package focus)

(use-package hideshow
  :bind
  ("M-s-h" . hs-toggle-hiding)
  :hook
  (prog-mode . hs-minor-mode))

(use-package smart-hungry-delete
  :config
  (smart-hungry-delete-add-default-hooks)
  :bind
  (:map prog-mode-map
        ("<backspace>" . smart-hungry-delete-backward-char)
        ("<delete>" . smart-hungry-delete-backward-char)
        ("C-d" . smart-hungry-delete-forward-char)))

(use-package ascii
  :load-path "~/.config/emacs/manually_installed"
  :commands (ascii-off ascii-on ascii-display)
  :bind ("C-c e" . ascii-toggle)
  :preface
  (defun ascii-toggle ()
    (interactive)
    (if ascii-display
        (ascii-off)
      (ascii-on))))

(use-package outline
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-highlight t))

(use-package conf-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist '("Cargo.lock\\'" . conf-toml-mode))
  (add-hook 'conf-mode-hook (lambda ()
                              "Set tab width to four spacess."
                              (setq-local tab-width 4)))
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)))

(use-package window
  :ensure nil
  :custom
  (split-height-threshold 60)
  (split-width-threshold 170)
  (split-window-preferred-function 'zino/split-window-sensibly)
  :config
  (defun zino/split-window-sensibly (&optional window)
    ;; Customized `split-window-sensibly' to prefer split horizontally.
    (let ((window (or window (selected-window))))
      (or (and (window-splittable-p window t)
	             ;; Try to split window horizontally first.
	             (with-selected-window window
	               (split-window-right)))
	        (and (window-splittable-p window)
	             ;; Split window vertically.
	             (with-selected-window window
	               (split-window-below)))
	        (and
           ;; If WINDOW is the only usable window on its frame (it is
           ;; the only one or, not being the only one, all the other
           ;; ones are dedicated) and is not the minibuffer window, try
           ;; to split it vertically disregarding the value of
           ;; `split-height-threshold'.
           (let ((frame (window-frame window)))
             (or
              (eq window (frame-root-window frame))
              (catch 'done
                (walk-window-tree (lambda (w)
                                    (unless (or (eq w window)
                                                (window-dedicated-p w))
                                      (throw 'done nil)))
                                  frame nil 'nomini)
                t)))
	         (not (window-minibuffer-p window))
	         (let ((split-height-threshold 0))
	           (when (window-splittable-p window)
	             (with-selected-window window
	               (split-window-below)))))))))

(use-package ediff
  :bind (("C-c = b" . ediff-buffers)
         ("C-c = B" . ediff-buffers3)
         ("C-c = c" . compare-windows)
         ("C-c = =" . ediff-files)
         ("C-c = f" . ediff-files)
         ("C-c = F" . ediff-files3)
         ("C-c = m" . count-matches)
         ("C-c = r" . ediff-revision)
         ("C-c = p" . ediff-patch-file)
         ("C-c = P" . ediff-patch-buffer)
         ("C-c = l" . ediff-regions-linewise)
         ("C-c = w" . ediff-regions-wordwise))
  :custom
  (ediff-combination-pattern
   '("<<<<<<< A: HEAD" A "||||||| Ancestor" Ancestor "=======" B ">>>>>>> B: Incoming")))

;; Try it some time.
;; (use-package sideline)
;; (use-package imenu-everywhere)
;; (use-package visual-regexp-steroids)
;; (use-package emacs-color-theme-solarized)
;; (use-package restclient)  ;; Test HTTP REST webservices
;; (use-package eros)
;; (use-package burly)
;; (use-package dired+)  ;; different colors for different file permissions
;; (use-package org-ioslide) ;; presentation
;; (use-package smart-hungry-delete)
;; (use-package popwin)
;; (use-package imenu-list)
;; (use-package git-gutter+)
;; (use-package flymake-popon)
;; (use-package flycheck-inline)
;; (use-package annotate)
;; (use-package forge)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:background "#2c3946" :foreground "#7bb6e2" :weight bold))))
 '(dictionary-word-definition-face ((t (:family "Fira Code"))))
 '(fixed-pitch ((t (:family "Fira Code" :height 250))))
 '(help-key-binding ((t (:inherit fixed-pitch :background "grey19" :foreground "LightBlue" :box (:line-width (-1 . -1) :color "grey35") :height 150))))
 '(next-error ((t (:inherit (bold region)))))
 '(pulse-highlight-face ((t nil)))
 '(pulse-highlight-start-face ((t nil)))
 '(tree-sitter-hl-face:property ((t (:inherit font-lock-constant-face))))
 '(variable-pitch ((t (:weight regular :height 180 :family "Fira Code"))))
 '(variable-pitch-text ((t (:inherit variable-pitch :height 1.0)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 5)
 '(bmkp-last-as-first-bookmark-file "~/.config/emacs/bookmarks" nil nil "Customized with use-package bookmark+")
 '(comment-style 'indent)
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "192.168.0.104")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(display-line-numbers-width-start nil)
 '(edebug-trace nil)
 '(fill-column 80)
 '(package-selected-packages
   '(explain-pause-mode json-rpc eglot eldoc-box flycheck-eglot nginx-mode git-modes screenshot magit nyan-mode orderless kind-icon corfu fish-completion esh-autosuggest pulsar crux helm-swoop bm avy-zap tree-sitter realgud god-mode magit-todos org-present company-lsp abbrev go-dlv elfeed json-mode nasm-mode flycheck-vale anki-editor flycheck-rust flycheck fzf consult helm expand-region gn-mode company-graphviz-dot graphviz-dot-mode org-remark rust-mode cape yaml-mode rime dired-rsync rg company org-roam-ui esup flymake-cursor mermaid-mode clipetty org lua-mode better-jumper org-notebook docker-tramp org-noter valign nov pdf-tools org-fragtog highlight-numbers rainbow-mode request beacon fixmee move-text go-mode popper cmake-mode dirvish fish-mode highlight-indent-guides indent-mode org-journal format-all filetags aggressive-indent agressive-indent elisp-format org-bars ws-butler emojify company-prescient prescien smartparents which-key visual-fill-column use-package undo-tree typescript-mode spacemacs-theme smartparens rainbow-delimiters pyvenv python-mode org-roam org-download org-bullets mic-paren lsp-ivy ivy-yasnippet ivy-xref ivy-rich ivy-prescient helpful helm-xref helm-lsp gruvbox-theme git-gutter general flycheck-pos-tip evil-visualstar evil-surround evil-leader evil-collection doom-themes doom-modeline counsel-projectile company-posframe company-fuzzy company-box command-log-mode clang-format ccls base16-theme))
 '(recenter-positions '(middle top bottom))
 '(safe-local-variable-values
   '((read-only . t)
     (comment-style quote box)
     (indent-bars-spacing-overide . 2)
     (indent-bars--offset . 2)
     (indent-bars-spacing-override . 2)
     (god-local-mode . t)
     (completion-styles orderless basic partial-completion))))

;; Run as daemon
(server-start)

;; Put this at the end otherwise `pcache' will still register itself
(remove-hook 'kill-emacs-hook 'pcache-kill-emacs-hook)

;;; init.el ends here
