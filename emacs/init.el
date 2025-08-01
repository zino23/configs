;; init.el --- startup file for emacs -*- lexical-binding: t -*-
;;
;;; commentary:
;; here is some magic that can grow hands on your mind, and vice versa.

;;; code:

(setq load-prefer-newer t)

;; Take the full control, don't load `default.el'.
(setq inhibit-default-init t)

(require 'package)

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu-devel" . "https://elpa.gnu.org/devel/")))

(setopt package-install-upgrade-built-in t)

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Must be set before loading `use-package'
(setq use-package-enable-imenu-support t)

(use-package use-package
  :custom
  (use-package-always-ensure t))

;; Immediately load this extension after loading `use-package'.
(use-package use-package-ensure-system-package
  :ensure nil)

(use-package system-packages
  :config
  (if (eq system-type 'darwin)
      (setq system-packages-package-manager 'port))
  (setq system-packages-use-sudo t))

(use-package emacs
  ;; Minimum settings use to debug a package.
  :init
  ;; On macos, treat option key as meta and command as super.
  (when (eq system-type 'darwin)
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super))
  (unless (display-graphic-p)
    (xterm-mouse-mode 1))
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (progn
        (message "Native compilation is available"))
    (message "Native complation is *not* available"))
  (setq user-emacs-directory "~/.config/emacs/")
  (setq native-comp-jit-compilation t)
  (setq native-comp-async-report-warnings-errors nil))

(use-package emacs
  :config
  ;; UI
  (if (display-graphic-p)
      (progn
        (scroll-bar-mode -1)
        (tool-bar-mode -1)
        (set-fringe-mode '(7 . 0))))
  ;; Display line numbers in mode line.
  (line-number-mode 1)
  (size-indication-mode -1)
  (tooltip-mode -1)
  (menu-bar-mode -1)
  (column-number-mode)
  (global-display-line-numbers-mode -1)
  (set-default-coding-systems 'utf-8)
  (global-visual-line-mode 1)
  ;; Keep buffers up to date.
  (global-auto-revert-mode t)
  ;; Remember and restore the last cursor location of opened files.
  (save-place-mode 1)
  (savehist-mode t)
  (recentf-mode t)
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil)
  ;; Disable line numbers for some modes.
  (dolist (mode '(term-mode-hook shell-mode-hook eshell-mode-hook pdf-view-mode-hook))
    (add-hook mode (lambda ()
                     (display-line-numbers-mode 0))))
  (setq next-screen-context-lines 2)
  (blink-cursor-mode)
  (setq blink-cursor-blinks 0)
  (repeat-mode)
  (pixel-scroll-mode 1)
  (delete-selection-mode 1)
  ;; autosave file-visiting buffer but not non-file-visiting e.g. *scratch*.
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

  ;; By default emacs create backup on first buffer save since the file is visited
  ;; C-u to save backup in second save
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
  ;; Put this at the end otherwise `pcache' will still register itself
  ;; (remove-hook 'kill-emacs-hook 'pcache-kill-emacs-hook)

  ;; Full screen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  ;; Natual Title Bar is disabled by default. Enable it by setting:
  ;; defaults write org.gnu.Emacs TransparentTitleBar DARK
  ;; Reference: https://github.com/railwaycat/homebrew-emacsmacport/wiki/Natural-Title-Bar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  :custom
  (confirm-kill-emacs 'yes-or-no-p)
  (read-process-output-max (* 1024 1024)) ;; 1 MB
  (inhibit-startup-screen t)
  ;; Dynamically adjust width
  (display-line-numbers-width nil)
  (display-line-numbers-width-start 0)
  (recenter-positions '(middle top bottom))
  (fill-column 80)
  (auto-revert-interval 5)
  (comment-style 'indent)
  ;; Set limit for prompt opening large files higher, 100 MB
  (large-file-warning-threshold 100000000)
  (custom-safe-themes t)
  (apropos-sort-by-scores t))

(use-package emacs
  :if (display-graphic-p)
  :config
  ;;; Check if a font exist:
  ;;; (member "Sarasa Mono SC Nerd" (font-family-list))
  ;; Set up fonts for different charsets.
  (defun my/set-font (font-name cn-font-name &optional initial-size cn-font-rescale-ratio)
    "Set different font-family for Latin and Chinese charactors."
    (let* ((size (or initial-size 14))
           (ratio (or cn-font-rescale-ratio 0.0))
           (main (font-spec :name font-name :size size))
           (cn (font-spec :name cn-font-name)))
      (set-face-attribute 'default nil :font main)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset cn))
      (setq face-font-rescale-alist (if (/= ratio 0.0) `((,cn-font-name . ,ratio)) nil))))

  ;; (my/set-font "Berkeley Mono" "Sarasa Mono SC Nerd" 14 1)
  (let*
      ((preferred-font "Berkeley Mono")
       (fallback-font "Iosevka NF")
       (font (if (member preferred-font (font-family-list))
                 preferred-font
               fallback-font)))
    (my/set-font font "LXGW WenKai" 21 1))
  
  (set-fontset-font
   t 'symbol
   (font-spec
    :family "Apple Color Emoji"
    :size 14
    :weight 'normal
    :width 'normal
    :slant 'normal)))

(use-package emacs
  ;; Performance tuning
  :config
  (setq-default bidi-display-reordering t)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; (setq-default cursor-in-non-selected-windows nil)
  (setq highlight-nonselected-windows nil)
  (setq fast-but-imprecise-scrolling t)
  ;; Raise gc threshold to boost startup.
  (setq gc-cons-percentage 0.5
        gc-cons-threshold (* 300 1024 1024))

  (add-hook 'after-init-hook (lambda ()
                               ;; Fallback to normal gc values.
                               (setq gc-cons-percentage 0.1)
                               (setq gc-cons-threshold (* 1000 1000)))))

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
  (help-window-select t)
  ;; Treat manual buffer switching the same as programmatic switching
  (switch-to-buffer-obey-display-actions t)
  (custom-file "~/.config/emacs/init.el")
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
  ;; > 100 for Never recenter when scrolling off-screen
  ;; 0 for always recenter when point is off screen
  (scroll-conservatively 0)
  ;; Needed to smoothly scrolll inline images
  (make-cursor-line-fully-visible nil)
  ;; Disable the visible bell
  (visible-bell nil)
  (next-error-found-function 'next-error-quit-window)
  (next-error-highlight t)
  (next-error-highlight-no-select t)
  (next-error-message-highlight t)
  (save-silently t)
  ;; Revert Dired and other buffers
  (global-auto-revert-non-file-buffers t)
  ;; Save minibuffer's history inputs
  (enable-recursive-minibuffers t)
  (history-length 25)
  ;; Delete files into trashbin
  (delete-by-moving-to-trash t)
  (trash-directory "~/.Trash")
  (async-shell-command-buffer 'confirm-kill-process)

  :custom-face
  (variable-pitch ((t (:height 1.2 :family "Bookerly"))))
  :config
  (setq ring-bell-function 'ignore)
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  (if (display-graphic-p)
      ;; https://stackoverflow.com/questions/4512075/how-to-use-ctrl-i-for-an-emacs-shortcut-without-breaking-tabs
      (keyboard-translate ?\C-i ?\H-i))

  (defun find-file--auto-create-dir (filename &optional _wildcards)
    "Create parent directory during visiting FILENAME if necessary.
Do not prompt me to create parent directory."
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t)))))

  (advice-add 'find-file :before 'find-file--auto-create-dir)

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
  )

(use-package emacs
  :config
  (defun my/toggle-window-dedication ()
    "Toggle window dedication in the selected window."
    (interactive)
    (set-window-dedicated-p (selected-window)
                            (not (window-dedicated-p (selected-window))))
    (if (window-dedicated-p (selected-window))
        (message "The current window is dedicated")
      (message "The current window is not dedicated")))
  (defun my/toggle-scratch ()
    "Toggle `scratch' buffer."
    (interactive)
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))

  (defun my/delete-to-end-of-buffer ()
    (interactive)
    (kill-region (point) (point-max)))
  (add-function :before after-focus-change-function 'garbage-collect)

  ;; Boxes comment
  ;; Site: https://boxes.thomasjensen.com/
  (defun my/boxes-create ()
    "Convert a region into box comments specified by `-d' option."
    (interactive)
    (shell-command-on-region (region-beginning)
                             (region-end) "boxes -d c-cmt2" nil 1 nil))

  (defun my/boxes-remove ()
    "Convert a region of box comments into plain code."
    (interactive)
    (shell-command-on-region (region-beginning)
                             (region-end) "boxes -r -d c-cmt2" nil 1 nil))

  ;;; File manipulation
  (defun my/insert-buffer-file-name (full-path)
    "Insert file name of the buffer at point.
Insert full path if prefix argument `FULL-PATH' is sent."
    (interactive "P")
    (let ((f (buffer-file-name)))
      (if (not full-path)
          (setq f (file-name-nondirectory f)))
      (insert f)))

  (defun my/inhibit-buffer-messages ()
    "Set `inhibit-message' buffer-locally."
    (setq-local inhibit-message t))

  (defun my/copy-full-path-to-kill-ring ()
    "Copy buffer's full path to kill ring."
    (interactive)
    (when buffer-file-name (kill-new (file-truename buffer-file-name))
          (message buffer-file-name)))

  (defun my/delete-till-end-of-buffer ()
    "Delete from point to the end of buffer."
    (interactive)
    (let ((beg (point)))
      (end-of-buffer)
      (kill-region beg (point))))

  (defun my/silence-advice (oldfun &rest args)
    (let ((inhibit-message t))
      (apply oldfun args)))

  ;; https://stackoverflow.com/questions/2289883/emacs-copy-matching-lines
  (defun my/copy-lines-matching-re (re)
    "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
    (interactive "sRegexp to match: ")
    (let ((result-buffer (get-buffer-create "*matching*")))
      (with-current-buffer result-buffer
        (erase-buffer))
      (save-match-data
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (princ (buffer-substring-no-properties (line-beginning-position)
                                                   (line-beginning-position 2))
                   result-buffer))))
      (pop-to-buffer result-buffer)))

  (defun my/scroll-other-window-down-one-line ()
    (interactive)
    (scroll-other-window-down 1))
  (defun my/scroll-other-window-one-line ()
    (interactive)
    (scroll-other-window 1))

  :bind
  ("C-c C-c d" . my/toggle-window-dedication)
  ("C-c C-z" . delete-to-end-of-buffer)
  ("C-c C-s" . my/toggle-scratch)
  ("C-s-k" . my/delete-till-end-of-buffer)
  ("C-M-S-n" . my/scroll-other-window-one-line)
  ("C-M-S-p" . my/scroll-other-window-down-one-line)
  :config
  ;; Mandatory, as the dictionary misbehaves!
  (add-to-list 'display-buffer-alist
               '("^\\*Dictionary\\*" display-buffer-in-side-window
                 (side . left)
                 (window-width . 50))))

(use-package emacs
  :config
  ;; TERMINAL MAPPINGS TO SUPPORT ITERM2 FOR MAC
  ;; reference: https://www.emacswiki.org/emacs/iTerm2
  ;; Also check the `kkp' package.
  (unless window-system
    (let ((map (if (boundp 'input-decode-map)
                   input-decode-map
                 function-key-map)))
      ;; Self-defined escape sequences so that:
      ;; 1. iterm2 send escape sequences when a keychord, e.g. "s-p" is pressed;
      ;; 2. emacs interprets the sequences as the kbd defined.
      (define-key map "\e[1;P0"  (kbd "s-l"))
      (define-key map "\e[1;P1"  (kbd "s-a"))
      (define-key map "\e[1;P2"  (kbd "s-d"))
      (define-key map "\e[1;P3"  (kbd "s-p"))
      (define-key map "\e[1;P4"  (kbd "s-."))
      (define-key map "\e[1;P5"  (kbd "s-j"))
      (define-key map "\e[1;P6"  (kbd "C-S-k"))
      (define-key map "\e[1;P7"  (kbd "C-s-<tab>"))
      (define-key map "\e[1;P8"  (kbd "s-b"))
      (define-key map "\e[1;P9"  (kbd "s-k"))
      (define-key map "\e[1;Q0"  (kbd "s-s"))
      (define-key map "\e[1;Q1"  (kbd "M-s-s"))
      (define-key map "\e[1;Q2"  (kbd "H-i"))
      (define-key map "\e[1;Q3"  (kbd "s-;"))
      (define-key map "\e[1;Q4"  (kbd "C-;"))
      (define-key map "\e[1;Q5"  (kbd "s-e"))
      (define-key map "\e[1;Q6"  (kbd "s-u"))
      (define-key map "\e[1;Q7"  (kbd "s-o"))
      (define-key map "\e[1;Q8"  (kbd "s-n"))
      (define-key map "\e[1;Q9"  (kbd "C-s-k"))
      (define-key map "\e[1;R1"  (kbd "s-<left>"))
      (define-key map "\e[1;R2"  (kbd "s-<right>"))
      (define-key map "\e[1;R3"  (kbd "s-<up>"))
      (define-key map "\e[1;R4"  (kbd "s-<down>"))
      (define-key map "\e[1;R5"  (kbd "C-M-S-v"))
      (define-key map "\e[1;R6"  (kbd "C-M-S-n"))
      (define-key map "\e[1;R7"  (kbd "C-M-S-p"))
      (define-key map "\e[1;R8"  (kbd "C-M-S-l"))
      (define-key map "\e[1;S1"  (kbd "C-,"))
      (define-key map "\e[1;S2"  (kbd "C-'"))
      (define-key map "\e[1;S3"  (kbd "C-s-f"))
      (define-key map "\e[1;S4"  (kbd "C-s-z"))
      (define-key map "\e[1;S5"  (kbd "C-M-,"))
      (define-key map "\e[1;S6"  (kbd "C-s-t"))
      (define-key map "\e[1;S7"  (kbd "C-s-x"))
      (define-key map "\e[1;S8"  (kbd "C-<return>"))
      (define-key map "\e[1;S9"  (kbd "C-M-i"))
      (define-key map "\e[1;T1"  (kbd "s-q"))
      (define-key map "\e[1;T2"  (kbd "s-'"))
      ;; Use iterm2's builtin feature:
      ;; xterm control sequence can enable modifyOtherKeysMode
      (define-key map "\e[85;7u" (kbd "C-s-c"))
      (define-key map "\e[86;7u" (kbd "C-s-v"))
      (define-key map "\e[87;7u" (kbd "C-s-k"))
      (define-key map "\e[88;7u" (kbd "C-s-n"))
      (define-key map "\e[89;7u" (kbd "C-s-p"))
      (define-key map "\e[91;7u" (kbd "C-:"))
      (define-key map "\e[92;7u" (kbd "M-s-<left>"))
      (define-key map "\e[93;7u" (kbd "M-s-<right>"))
      (define-key map "\e[96;7u" (kbd "C-<backspace>"))
      (define-key map "\e[97;7u" (kbd "C-M-a"))
      (define-key map "\e[98;7u" (kbd "C-M-b"))
      (define-key map "\e[99;7u" (kbd "M-s-5"))
      (define-key map "\e[100;7u" (kbd "C-M-d"))
      (define-key map "\e[101;7u" (kbd "C-M-e"))
      (define-key map "\e[102;7u" (kbd "C-M-f"))
      (define-key map "\e[103;7u" (kbd "C-s-z"))
      (define-key map "\e[117;7u" (kbd "C-M-u"))
      (define-key map "\e[118;7u" (kbd "C-M-i"))
      ;; (define-key map "\e[1;P12" (kbd "H-d"))
      ;; (define-key map "\e[1;P13" (kbd "H-e"))
      ;; (define-key map "\e[1;P14" (kbd "H-f"))
      ;; (define-key map "\e[1;P15" (kbd "H-g"))
      ;; (define-key map "\e[1;P16" (kbd "H-h"))
      ;; (define-key map "\e[1;P17" (kbd "H-i"))
      ;; (define-key map "\e[1;P18" (kbd "H-j"))
      ;; (define-key map "\e[1;P19" (kbd "H-k"))
      ;; (define-key map "\e[1;P20" (kbd "H-l"))
      ;; (define-key map "\e[1;P21" (kbd "H-m"))
      ;; (define-key map "\e[1;P22" (kbd "H-n"))
      ;; (define-key map "\e[1;P23" (kbd "H-o"))
      ;; (define-key map "\e[1;P24" (kbd "H-p"))
      ;; (define-key map "\e[1;P25" (kbd "H-q"))
      ;; (define-key map "\e[1;P26" (kbd "H-r"))
      ;; (define-key map "\e[1;P27" (kbd "H-s"))
      ;; (define-key map "\e[1;P28" (kbd "H-t"))
      ;; (define-key map "\e[1;P29" (kbd "H-u"))
      ;; (define-key map "\e[1;P30" (kbd "H-v"))
      ;; (define-key map "\e[1;P31" (kbd "H-w"))
      ;; (define-key map "\e[1;P32" (kbd "H-x"))
      ;; (define-key map "\e[1;P33" (kbd "H-y"))
      ;; (define-key map "\e[1;P34" (kbd "H-z"))
      ;; (define-key map "\e[1;P35" (kbd "H-0"))
      ;; (define-key map "\e[1;P36" (kbd "H-1"))
      ;; (define-key map "\e[1;P37" (kbd "H-2"))
      ;; (define-key map "\e[1;P38" (kbd "H-3"))
      ;; (define-key map "\e[1;P39" (kbd "H-4"))
      ;; (define-key map "\e[1;P40" (kbd "H-5"))
      ;; (define-key map "\e[1;P41" (kbd "H-6"))
      ;; (define-key map "\e[1;P42" (kbd "H-7"))
      ;; (define-key map "\e[1;P43" (kbd "H-8"))
      ;; (define-key map "\e[1;P44" (kbd "H-9"))
      ;; (define-key map "\e[1;P45" (kbd "H-<f1>"))
      ;; (define-key map "\e[1;P46" (kbd "H-<f2>"))
      ;; (define-key map "\e[1;P47" (kbd "H-<f3>"))
      ;; (define-key map "\e[1;P48" (kbd "H-<f4>"))
      ;; (define-key map "\e[1;P49" (kbd "H-<f5>"))
      ;; (define-key map "\e[1;P50" (kbd "H-<f6>"))
      ;; (define-key map "\e[1;P51" (kbd "H-<f7>"))
      ;; (define-key map "\e[1;P52" (kbd "H-<f8>"))
      ;; (define-key map "\e[1;P53" (kbd "H-<f9>"))
      ;; (define-key map "\e[1;P54" (kbd "H-<f10>"))
      ;; (define-key map "\e[1;P55" (kbd "H-<f11>"))
      ;; (define-key map "\e[1;P56" (kbd "H-<f12>"))
      )))

;; Need to use the one from elpa. The shipped version is too low for `magit'.
(use-package transient :ensure t)

(use-package pixel-scroll
  :disabled
  :ensure nil
  :bind
  ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :custom
  (pixel-scroll-precision-interpolate-page t)
  :init
  (pixel-scroll-precision-mode 1))

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

(global-set-key (kbd "s-b") 'my/switch-other-buffer)

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
                  (my/smart-join-line -1)))

;; Taken from https://tony-zorman.com/posts/join-lines-comments.html.
(defun my/smart-join-line (&optional arg beg end)
  "Join this line to previous and fix up whitespace at join.
If there is a fill prefix, delete it from the beginning of this
line.
With prefix ARG, join the current line to the following line.
When BEG and END are non-nil, join all lines in the region they
define.  Interactively, BEG and END are, respectively, the start
and end of the region if it is active, else nil.  (The region is
ignored if prefix ARG is given.)

When joining lines, smartly delete comment beginnings, such that one
does not have to do this by oneself."
  (interactive
   (progn (barf-if-buffer-read-only)
          (cons current-prefix-arg
                (and (use-region-p)
                     (list (region-beginning) (region-end))))))
  ;; Consistently deactivate mark even when no text is changed.
  (setq deactivate-mark t)
  (if (and beg (not arg))
      ;; Region is active.  Go to END, but only if region spans
      ;; multiple lines.
      (and (goto-char beg)
           (> end (line-end-position))
           (goto-char end))
    ;; Region is inactive.  Set a loop sentinel
    ;; (subtracting 1 in order to compare less than BOB).
    (setq beg (1- (line-beginning-position (and arg 2))))
    (when arg (forward-line)))
  (let ((prefix (and (> (length comment-start) 0)
                     (regexp-quote comment-start))))
    (while (and (> (line-beginning-position) beg)
                (forward-line 0)
                (= (preceding-char) ?\n))
      (if (save-excursion (forward-line -1) (eolp))
          (delete-char -1)
        (delete-char -1)
        ;; If the appended line started with the fill prefix, delete it.
        (let ((prev-comment?            ; Don't delete the start of a comment.
               (save-excursion
                 (back-to-indentation)
                 (looking-at prefix))))
          (delete-horizontal-space)
          (while (and prev-comment? prefix (looking-at prefix))
            (replace-match "" t t))
          (fixup-whitespace))))))

(defun my/toggle-window-split ()
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

(define-key ctl-x-4-map "t" 'my/toggle-window-split)

;; The following only undoes remapping instead of remapping to nil
;; (define-key global-map [remap ns-print-buffer] nil)
(global-unset-key (kbd "s-p"))

;; the original keybinding is set-goal-column
(global-unset-key (kbd "C-x C-n"))

(global-set-key (kbd "C-c +") 'my/increment-number-decimal)

(global-set-key (kbd "C-d") 'my/delete-forward-char)

(defun my/delete-forward-char (n)
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

(defun my/next-k-lines ()
  "Move cursor down k lines."
  (interactive)
  (scroll-up 5))

(defun my/previous-k-lines ()
  "Move cursor up k lines."
  (interactive)
  (scroll-down 5))

(global-set-key (kbd "M-n") 'my/next-k-lines)
(global-set-key (kbd "M-p") 'my/previous-k-lines)
;; (global-set-key (kbd "C-,") 'scroll-up-command)
;; (global-set-key (kbd "C-.") 'scroll-down-command)

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "M-e") 'forward-word)

(defun my/kill-whole-line-without-newline ()
  "Kill the whole line but leave the trailing newline."
  (interactive)
  (kill-whole-line 0))

(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-S-k") 'kill-line)
(global-set-key (kbd "C-S-s-k") 'my/kill-whole-line-without-newline)

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
(defun my/switch-other-buffer ()
  "Swithch to the most recent buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "C-c b") 'my/switch-other-buffer)
(global-set-key (kbd "C-c C-b") 'my/switch-other-buffer)

(global-set-key (kbd "<RET>") 'newline-and-indent)

;; Automatically insert comment prefix and indent if inside comments.
(global-set-key
 [remap newline]
 `(menu-item "" default-indent-new-line :filter
             ,(lambda (_cmd)
                (when (save-excursion (comment-beginning))
                  `(lambda () (interactive) (,comment-line-break-function))))))

(defun my/save-buffer-and-exit()
  "Simple convenience function.
Save the buffer of the current window and kill it"
  (interactive)
  (save-buffer)
  (delete-window))
(global-set-key (kbd "C-x s-s") #'my/save-buffer-and-exit)

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

(use-package edebug
  :after eros
  :ensure nil
  :config
  (defun adviced:edebug-previous-result (_ &rest r)
    "Adviced `edebug-previous-result'."
    (eros--make-result-overlay edebug-previous-result
      :where (point)
      :duration eros-eval-result-duration))

  (advice-add #'edebug-previous-result
              :around
              #'adviced:edebug-previous-result)

  (defun adviced:edebug-compute-previous-result (_ &rest r)
    "Adviced `edebug-compute-previous-result'."
    (let ((previous-value (nth 0 r)))
      (if edebug-unwrap-results
          (setq previous-value
                (edebug-unwrap* previous-value)))
      (setq edebug-previous-result
            (edebug-safe-prin1-to-string previous-value))))

  (advice-add #'edebug-compute-previous-result
              :around
              #'adviced:edebug-compute-previous-result)
  :bind
  (:map edebug-mode-map
        ("C-c C-d" . helpful-at-point)))

;; Reference: https://emacs.stackexchange.com/a/7670/37427
;; Edebug a defun or defmacro
(defvar my/fns-in-edebug nil
  "List of functions for which `edebug' is instrumented.")

(defconst my/fns-regexp (concat "(\\s-*"
                                "\\(defun\\|defmacro\\)\\s-+"
                                "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
  "Regexp to find defun or defmacro definition.")

(defun my/toggle-edebug-defun ()
  "Toggle instrumenting a function or macro for edebug."
  (interactive)
  (let (fn)
    (save-excursion
      (search-backward-regexp my/fns-regexp)
      (setq fn (match-string 1))
      (mark-sexp)
      (narrow-to-region (point) (mark))
      (pop-mark)
      (if (member fn my/fns-in-edebug)
          ;; If the function is already being edebugged, uninstrument it
          (progn
            (setq my/fns-in-edebug (delete fn my/fns-in-edebug))
            (eval-region (point) (mark))
            (setq-default eval-expression-print-length 12)
            (setq-default eval-expression-print-level  4)
            (message "Edebug disabled: %s" fn))
        ;; If the function is not being edebugged, instrument it
        (progn
          (add-to-list 'my/fns-in-edebug fn)
          (setq-default eval-expression-print-length nil)
          (setq-default eval-expression-print-level  nil)
          (edebug-defun)
          (message "Edebug: %s" fn)))
      (widen))))

(global-set-key (kbd "C-s-x") 'my/toggle-edebug-defun)

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
  ;; The ability to restore a frame deleted by accident.
  ;; FIXME: also restore frame name.
  (undelete-frame-mode)
  :bind
  ("C-c f" . frame-map)
  ("C-c f r" . set-frame-name)
  ("C-c f s" . select-frame-by-name)
  ("C-M-<tab>" . select-frame-by-name))

(use-package minibuffer
  :ensure nil
  :bind
  (:map minibuffer-mode-map
        ("s-n" . other-window-prefix))
  :config
  (add-hook 'minibuffer-mode-hook (defun my/control-g-directly-exit-in-minibuffer ()
                                    (delete-selection-mode -1))))

(use-package elisp-mode
  :ensure nil)

(use-package elisp-def
  :hook
  (emacs-lisp-mode . elisp-def-mode)
  (helpfule-mode . elisp-def-mode)
  :config
  (advice-add 'elisp-def :after 'beacon-blink))

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

(use-package hl-line
  :ensure nil
  :init
  (global-hl-line-mode)
  :config
  (add-hook 'vterm-mode-hook (lambda ()
                               "Disable `global-hl-line-mode' locally."
                               (setq-local global-hl-line-mode nil)))

  (add-hook 'activate-mark-hook (lambda ()
                                  ;; `hl-line-mode' works by putting an overlay on the current line.
                                  (if global-hl-line-overlay
                                      (delete-overlay global-hl-line-overlay))
                                  (setq-local global-hl-line-mode nil)))

  (add-hook 'deactivate-mark-hook (lambda ()
                                    (setq-local global-hl-line-mode t)))

  ;; For `doom-one' theme.
  ;; (set-face-attribute 'hl-line nil :inherit nil :background "#21242b") ;;"#2e3b49")
  ;; (set-face-attribute 'region nil :inherit nil :distant-foreground "#959ba5" :background "#42444a")
  )

(use-package mouse
  :ensure nil
  :config
  (add-hook 'mouse-leave-buffer-hook (lambda ()
                                       (if (and global-hl-line-mode global-hl-line-overlay)
                                           (delete-overlay global-hl-line-overlay)))))

(use-package warnings
  :ensure nil
  :custom
  (warning-minimum-level :error))

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
    (setq my/abbrev-table '(("tset" "test")
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
                            ("contorl" "control")
                            ("requset" "request")
                            ("reqeust" "request")
                            ("triat" "trait")
                            ("imme" "immediately")
                            ))
    (dolist (pair my/abbrev-table)
      (let ((from (car pair))
            (to (car (cdr pair))))
        (define-abbrev global-abbrev-table from to)))))

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
        ("M-o" . ace-window)
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
   ("<DEL>" . dired-up-directory)
   ;; `dired-goto-file' is so useless.
   ("j" . find-file))
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-hide-details-mode)
  ;; Omit uninterested files
  (dired-mode . dired-omit-mode)
  ;; Drag-and-drop to `dired'
  (dired-mode . org-download-enable)
  (dired-mode . dired-async-mode)
  :custom
  (dired-dwim-target t)
  (dired-create-destination-dirs 'always)
  (auto-revert-verbose nil)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-omit-verbose nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-hide-details-hide-information-lines nil)
  (dired-mouse-drag-files 'move)
  :config
  (setq ls-lisp-use-insert-directory-program t)
  ;; In MacOS `ls' does not have `--group-directories-first' option, use `gls'.
  (if (and (and (eq system-type 'darwin)
                (eq 0 (shell-command "which gls"))))

      (progn
        (setq insert-directory-program (string-trim (shell-command-to-string "which gls")))
        (setq dired-listing-switches "-alGuh --group-directories-first"))))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :commands (diredfl-global-mode)
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

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

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
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (eldoc-idle-delay 0.1)
  (eldoc-echo-area-prefer-doc-buffer t)
  (max-mini-window-height 0.2)
  (help-at-pt-timer-delay 1)
  (help-at-pt-display-when-idle '(flymake-iagnostics))
  :config
  :bind
  ("C-h C-." . eldoc-doc-buffer)
  :custom-face
  ;;; Customize the doc face.
  ;;; For `solarizied-gruvbox-dark'
  ;; (markdown-code-face ((t (:foreground "#7c6f64" :family "Fira Code"))))

  ;;; For `zenburn'
  (markdown-code-face ((t (:family "Berkeley Mono")))))

(use-package eldoc-box
  :commands (eldoc-box-help-at-point)
  :bind
  (:map eglot-mode-map
        ("C-c s-d" . eldoc-box-help-at-point))
  ;; :custom-face
  ;; (eldoc-box-border ((t (:background "#3c586f" :weight heavy))))
  ;; (eldoc-box-border ((t (:background "#51afef" :weight bold))))
  )

;; Automatically trim trailing whitespaces.
(use-package ws-butler
  :config
  (ws-butler-global-mode))

;; Format code while editing lisp code
(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package rainbow-mode
  :hook
  (emacs-lisp-mode . rainbow-mode)
  (lispy-data-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (lispy-data-mode . rainbow-delimiters-mode)
  (debugger-mode . rainbow-delimiters-mode)
  (dired-preview-mode . rainbow-delimiters-mode)
  (conf-toml-mode . rainbow-delimiters-mode)
  (git-timemachine-mode . rainbow-delimiters-mode))

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
  (show-paren-style 'parenthesis)
  ;; :custom-face
  ;; (show-paren-match-expression ((t (:inherit nil :background "#282c34" :weight bold))))
  )

(use-package nerd-icons
  ;; Install a Nerd font on https://www.nerdfonts.com/font-downloads, and set `nerd-icons-font-family' to
  ;; match the font name
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package solarized-theme
  :disabled
  :init
  (load-theme 'solarized-gruvbox-dark t))

(use-package gruvbox-theme
  :disabled
  :init
  (load-theme 'gruvbox))

(use-package modus-themes
  :disabled
  :init
  (load-theme 'modus-vivendi-tinted)
  :config
  (setq modus-themes-region '(bg-only)))

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t)
  (set-face-attribute 'eldoc-highlight-function-argument nil :background "#5F5F5F"))

(use-package zenburn-theme
  :init
  (require 'avy)
  (require 'vertico-quick)
  :custom-face
  (avy-goto-char-timer-face ((t (:inherit isearch :foreground "#D0BF8F"))))
  (avy-lead-face ((t :inherit vertico-quick1 :foreground unspecified :background unspecified)))
  (avy-lead-face-0 ((t :inherit vertico-quick2 :foreground unspecified :background unspecified)))
  (avy-lead-face-1 ((t :inherit bold :foreground "white" :background "#FFA500")))
  (avy-lead-face-2 ((t :inherit bold :foreground "white" :background "#228B22"))))

(use-package zenburn-theme
  :after corfu
  :custom-face
  ;; Darker current candidate.
  ;; (corfu-current ((t (:foreground "#DCDCCC" :background "#4F4F4F"))))
  ;; (corfu-default ((t (:foreground "#DCDCCC" :background "#2B2B2B"))))
  ;; (corfu-popupinfo ((t (:inherit corfu-current))))

  ;; Ligher current candidate.
  (corfu-current ((t (:foreground "#DCDCCC" :background "#2B2B2B"))))
  (corfu-default ((t (:foreground "#DCDCCC" :background "#4F4F4F"))))
  (corfu-popupinfo ((t (:inherit corfu-default)))))

(use-package zenburn-theme
  :after eglot
  :custom-face
  (eglot-inlay-hint-face ((t (:inherit shadow :family "Iosevka" :height 0.9))))
  (eglot-parameter-hint-face ((t (:inherit eglot-inlay-hint-face :family "Iosevka"))))
  (eglot-type-hint-face ((t (:inherit eglot-inlay-hint-face :family "Iosevka")))))

(use-package zenburn-theme
  :after (rustic vterm)
  :config
  (setq rustic-ansi-faces (let ((faces (list)))
                            (dolist (face '(term-color-black
                                            term-color-red
                                            term-color-green
                                            term-color-yellow
                                            term-color-blue
                                            term-color-magenta
                                            term-color-cyan))
                              (add-to-list 'faces (face-foreground face) t))
                            (vconcat faces))))

(use-package zenburn-theme
  :after org
  :custom-face
  (org-level-1 ((t (:inherit default :extend nil :foreground "#DFAF8F" :height 1.3 :family "Iosevka"))))
  (org-level-2 ((t (:inherit default :extend nil :foreground "#BFEBBF" :height 1.2 :family "Iosevka"))))
  ;; (org-level-3 ((t (:inherit default :extend nil :foreground "#7CB8BB" :height 1.1 :family "Iosevka"))))
  ;; (org-level-4 ((t (:inherit default :extend nil :foreground "#D0BF8F" :height 1.05 :family "Iosevka"))))
  ;; (org-level-5 ((t (:inherit default :extend nil :foreground "#93E0E3" :family "Iosevka"))))
  ;; (org-level-6 ((t (:inherit default :extend nil :foreground "#9FC59F" :family "Iosevka"))))
  (org-level-3 ((t (:inherit default :extend nil :foreground "#9FC59F" :height 1.1 :family "Iosevka"))))
  (org-level-4 ((t (:inherit default :extend nil :foreground "#D0BF8F" :height 1.05 :family "Iosevka"))))
  (org-level-5 ((t (:inherit default :extend nil :foreground "#DFAF8F" :family "Iosevka"))))
  (org-level-6 ((t (:inherit default :extend nil :foreground "#BFEBBF" :family "Iosevka"))))
  (org-code ((t (:foreground "#DFAF8F"))))
  ;; (org-verbatim ((t (:inherit font-lock-doc-face))))
  ;; (org-verbatim ((t (:inherit font-lock-doc-face :foreground "#ECB3B3"))))
  ;; (org-verbatim ((t (:inherit font-lock-doc-face :foreground "#94BFF3"))))
  (org-verbatim ((t (:inherit font-lock-doc-face :foreground "#8CD0D3"))))
  (org-done ((t (:weight bold :weight bold :foreground "#6F6F6F"))))
  (org-headline-done ((t (:foreground "#6F6F6F")))))

(use-package zenburn-theme
  :after org-remark
  :custom-face
  ;; (org-remark-highlighter ((t (:background "#4C7073" :underline nil))))
  (org-remark-highlighter ((t (:background "#5F5F5F" :underline nil))))
  ;; (org-remark-highlighter ((t (:background "#4F5B66" :underline nil))))
  ;; (org-remark-highlighter ((t (:background "#808000" :underline nil))))
  )

(use-package zenburn-theme
  :after diredfl
  :config
  (set-face-attribute 'diredfl-dir-name nil :foreground "#7474FFFFFFFF" :background 'unspecified))

(use-package zenburn-theme
  :after dired-subtree
  :config
  (let ((depth 5))
    (cl-loop for i from 1 to depth
             for face = (intern (format "dired-subtree-depth-%d-face" i))
             do
             (set-face-attribute face nil :background 'unspecified))))

(use-package zenburn-theme
  :after consult
  :custom-face
  (consult-imenu-prefix ((t (:inherit consult-async-running :weight normal)))))

(use-package zenburn-theme
  :after bookmark+
  :config
  (set-face-attribute 'bmkp-local-directory nil :background 'unspecified :weight 'normal)
  (set-face-attribute 'bmkp-local-file-without-region nil :foreground (face-foreground 'default) :weight
                      'normal)
  :custom-face
  (bmkp-light-non-autonamed-region ((t (:background "#5F5F5F")))))

(use-package zenburn-theme
  :after rainbow-delimiters
  :config
  ;; Fix colorless foreground of `rainbow-delimiters-depth-1-face'.
  (let ((depth 9))
    (cl-loop for i from 2 to depth
             for face = (intern (format "rainbow-delimiters-depth-%d-face" i))
             do
             ;; Assign `rainbow-delimiters-depth-{i}-face' one level deeper, i.e. `rainbow-delimiters-depth-{i+1}-face'.
             (set-face-attribute (intern (format "rainbow-delimiters-depth-%d-face" (1- i))) nil :foreground
                                 (face-foreground face)))))

(use-package zenburn-theme
  :after orderless
  :custom-face
  (orderless-match-face-0 ((t :foreground "#7F9F7F" :inherit bold)))
  (orderless-match-face-1 ((t :foreground "#DC8CC3" :inherit bold)))
  (orderless-match-face-2 ((t :foreground "#8CD0D3" :inherit bold)))
  (orderless-match-face-3 ((t :foreground "#D0BF8F" :inherit bold))))

(use-package doom-modeline
  :load-path "site-lisp/doom-modeline"
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
  (doom-modeline-buffer-modified ((t (:background unspecified :inherit (bold))))))

(use-package doom-modeline
  :if (eq window-system 'pgtk)
  :config
  ;; NOTE: This does not work.  We have to define our own segment.
  ;;
  ;; Override attributes of the face used for padding.
  ;; If the space character is very thin in the modeline, for example if a
  ;; variable pitch font is used there, then segments may appear unusually close.
  ;; To use the space character from the `fixed-pitch' font family instead, set
  ;; this variable to `(list :family (face-attribute 'fixed-pitch :family))'.
  ;; (setq doom-modeline-spc-face-overrides (propertize " " 'face (doom-modeline-spc-face) 'display '((space :relative-width 1.0))))
  (defsubst my/doom-modeline-vspc ()
    (propertize " "
                'face (doom-modeline-spc-face)
                'display '((space :relative-width 1))))

  (defsubst my/doom-modeline--buffer-mode-icon ()
    "The icon of the current major mode."
    (when (and doom-modeline-icon doom-modeline-major-mode-icon)
      (when-let* ((icon (or doom-modeline--buffer-file-icon
                            (doom-modeline-update-buffer-file-icon))))
        (unless (string-empty-p icon)
          (concat
           (if doom-modeline-major-mode-color-icon
               (doom-modeline-display-icon icon)
             (doom-modeline-propertize-icon
              icon
              (doom-modeline-face)))
           (my/doom-modeline-vspc))))))

  (defsubst my/doom-modeline-buffer-state-icon ()
    (when doom-modeline-buffer-state-icon
      (when-let* ((icon (doom-modeline-update-buffer-file-state-icon)))
        (unless (string-empty-p icon)
          (concat
           (doom-modeline-display-icon icon)
           (my/doom-modeline-vspc))))))

  (doom-modeline-def-segment my/buffer-info
    (concat
     (doom-modeline-spc)
     (my/doom-modeline--buffer-mode-icon)
     (my/doom-modeline-buffer-state-icon)
     (doom-modeline--buffer-name)))

  (doom-modeline-def-segment my/check
    "Displays color-coded error status in the current buffer with pretty icons."
    (when-let* ((sep (doom-modeline-spc))
                (vsep (my/doom-modeline-vspc))
                (seg (cond
                      ((and (bound-and-true-p flymake-mode)
                            (bound-and-true-p flymake--state)) ; only support 26+
                       doom-modeline--flymake)
                      ((and (bound-and-true-p flycheck-mode)
                            (bound-and-true-p flycheck--automatically-enabled-checkers))
                       doom-modeline--flycheck))))
      (concat
       sep
       (let ((str))
         (dolist (s (split-string seg " "))
           (setq str
                 (concat str
                         (if (string-match-p "^[0-9]+$" s)
                             (concat vsep
                                     (doom-modeline-display-text s)
                                     vsep)
                           (doom-modeline-display-icon s)))))
         (propertize str
                     'help-echo (get-text-property 0 'help-echo seg)
                     'mouse-face 'doom-modeline-highlight
                     'local-map (get-text-property 0 'local-map seg)))
       sep)))

  (doom-modeline-def-modeline 'my/doom-modeline-main
    '(eldoc bar window-state workspace-name window-number modals matches follow my/buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info project-name persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs my/check time))
  (doom-modeline-set-modeline 'my/doom-modeline-main 'default))

(use-package doom-themes
  :disabled
  :init
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-tokyo-night t)
  ;; (load-theme 'doom-solarized-dark-high-contrast t)
  ;; (load-theme 'doom-gruvbox t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  )

(use-package doom-one-theme
  :disabled
  :ensure nil
  :after doom-themes
  :init
  (load-theme 'doom-one t)
  :custom-face
  (bmkp-local-file-without-region ((t (:foreground "#bbc2cf"))))
  (org-remark-highlighter ((t (:background "#023047" :underline nil)))))

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
   ;; unwrap the enclosing sexp
   ("M-r" . sp-splice-sexp)
   ;; delete anything of the enclosing sexp
   ("M-k" . sp-change-enclosing)
   ;; rewrap with a different pair
   ("C-M-r" . sp-rewrap-sexp)
   ;; copy the sexp at point
   ("M-y" . sp-copy-sexp)
   ;; kill the sexp at point
   ("C-M-k" . sp-kill-sexp)
   ;; remove the last sexp from current list by moving the closing delimiter
   ("C-c s b" . sp-forward-barf-sexp)
   ;; eat the next sexp into current one
   ("C-c s s" . sp-forward-slurp-sexp)
   ;; unwrap the next sexp. 'n' as in 'next'
   ("C-c s n" . sp-unwrap-sexp)
   ;; unwrap the current list. 'c' as in 'current'
   ("C-c s c" . sp-splice-sexp)
   ;; clear the inside of the enclosing sexp, like vim's <ci>
   ("C-c s k" . sp-change-enclosing))

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
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))

  :bind
  (
   ;; It is quite common to walk a sexp and immediately start editing, so the active repeat map will be too
   ;; disturbing as we have to manually press `C-g' to opt out of repeat mode keybindings.
   :repeat-map
   smartparens-mode-repeat-map
   ;;  ("A" . sp-beginning-of-sexp)
   ;;  ("E" . sp-end-of-sexp)
   ;;  ("f" . sp-forward-sexp)
   ;;  ("b" . sp-backward-sexp)
   ;;  ("d" . sp-down-sexp)
   ;;  ("e" . sp-up-sexp)
   ;;  ("a" . sp-backward-down-sexp)
   ;;  ("u" . sp-backward-up-sexp)
   ;;  ("n" . sp-next-sexp)
   ;;  ("p" . sp-previous-sexp)
   ("B" . sp-forward-barf-sexp)
   ("s" . sp-forward-slurp-sexp)
   ;;  ("k" . sp-change-enclosing)
   ;;  ("c" . sp-splice-sexp)
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
  :init
  (vertico-multiform-mode 1)
  :custom
  ;; Configure the display per command.
  ;; Use a buffer with indices for imenu
  ;; and a flat (Ido-like) menu for M-x.
  (vertico-multiform-commands '((consult-imenu buffer indexed)
                                (consult-ripgrep buffer indexed)
                                (my/consult-ripgrep-thing-at-point buffer indexed)
                                (consult-imenu-multi buffer indexed)))
  :bind
  (:map vertico-multiform-map
        ("M-B" . vertico-multiform-buffer)
        ("M-F" . vertico-multiform-flat)
        ("M-G" . vertico-multiform-grid)
        ("M-U" . vertico-multiform-unobstrusive)
        ("M-V" . vertico-multiform-vertical)))

(use-package vertico-quick
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
        ("s-j" . vertico-quick-jump)
        ;; Exit vertico with the selected candidate
        ("M-q" . vertico-quick-exit)
        ("C-M-q" . vertico-quick-insert)))

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
  ([remap describe-function] . helpful-function)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ([remap describe-macro] . helpful-macro)
  ([remap describe-symbol] . helpful-symbol)
  ("C-c C-d" . helpful-at-point)
  ("C-h M-f" . describe-face)
  (:map helpful-mode-map
        ;; Already got <tab> for `next-button'. Often times being able to
        ;; scroll the screen with single hand is nice.
        ("n" . next-line)
        ("p" . previous-line)
        ("f" . forward-char)
        ("b" . backward-char)))

(use-package projectile
  :config
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/dev")
    (add-to-list 'projectile-project-search-path "~/dev/"))
  (when (file-directory-p "~/gitlab")
    (add-to-list 'projectile-project-search-path "~/gitlab/"))
  ;; [use] call `projectile-find-file' with prefix argument will invalidate cache first
  (setq projectile-switch-project-action #'projectile-find-file)
  :custom
  ;; `alien' indexing method ignores patterns listed in .gitignore, but does not
  ;; respect .projectile
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  (projectile-globally-ignored-directories nil) ; quick fix for bbatsov/projectile#1777
  (projectile-require-project-root t)
  :bind-keymap
  ("s-p" . projectile-command-map)
  :config
  (projectile-mode)
  (let ((map (make-sparse-keymap)))
    (fset 'projectile-replace-command-map map)
    (define-key projectile-command-map (kbd "r") 'projectile-replace-command-map)
    (define-key map (kbd "l") 'projectile-replace)
    (define-key map (kbd "r") 'projectile-replace-regexp)))

;; Boost performance of `magit'
;; `libegit2' has stalled since 2023, need to confirm its usability.
;; (use-package libgit
;;   :load-path "site-lisp/libegit2/"
;;   :custom
;;   (libgit-auto-rebuild t))

;; From tarius: since 2020 Melpa distributes magit and magit-section as two separate packages, so that other
;; packages can depend on the latter without pulling in all of the former. magit-section is specified as a
;; dependency of magit and your package manager was supposed to install magit-section when you requested that
;; it install magit. If it failed to do that, you can fix it by explicitly installing magit-section.
(use-package magit-section)

(use-package magit
  :after magit-section
  :bind
  (("C-x g" . magit-status)
   ([remap vc-diff] . magit-diff-buffer-file)
   :map magit-stash-mode-map
   ("M-n" . my/next-k-lines)
   ("M-p" . my/previous-k-lines)
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
  (magit-process-popup-time -1)
  ;; Only refresh the current magit buffer
  (magit-refresh-status-buffer nil)
  (magit-save-repository-buffers 'dontask)
  (magit-process-popup-time 10 "pop up process buffer is a command takes longer than this many seconds")
  (magit-no-confirm
   '(stage-all-changes unstage-all-changes rename set-and-push))
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
    (remove-hook 'magit-status-sections-hook func))

  ;; Reverse the removal.
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
                  ;; magit-insert-recent-commits
                  ))
    (add-hook 'magit-status-sections-hook func))

  (dolist (func '(
                  ;; magit-insert-error-header
                  magit-insert-diff-filter-header
                  ;; magit-insert-head-branch-header
                  ;; magit-insert-upstream-branch-header
                  ;; magit-insert-push-branch-header
                  magit-insert-tags-header
                  ))
    (remove-hook 'magit-status-headers-hook func)))

(use-package diff-hl
  :config
  (advice-add 'diff-hl-next-hunk :after (lambda (&optional backward)
                                          (recenter)))
  :custom
  (diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup)
  (diff-hl-show-staged-changes nil))

(use-package org-roam
  :if (sqlite-available-p)
  :config
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

  :custom
  (org-roam-directory "~/Notes")
  (org-roam-dailies-directory "Journal/")
  (org-roam-completion-everywhere nil)
  (org-roam-capture-templates
   '(("c" "Default" entry "* %?"
      :target (file+head "${slug}.org" "#+TITLE: ${slug}\n#+FILETAGS: %^{tags}\n#+CREATED: %<%Y-%m-%d>\n#+STARTUP: fold")
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
  (defun my/org-roam-node-find-other-window ()
    (interactive)
    (org-roam-node-find 'other-windw))
  :config
  (global-unset-key (kbd "s-n"))
  (global-set-key (kbd "s-n f") 'org-roam-node-find)
  ;; Keybindings containing `4' before normal key actions often means to execute
  ;; the action in other window.
  (global-set-key (kbd "s-n 4 f") 'my/org-roam-node-find-other-window))

(use-package magit-todos
  ;; Not compatible with Emacs 28
  :disabled
  :cusyom
  (magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'"))

(use-package magit-delta
  :after magit
  :if (eq 0 (shell-command "which delta"))
  :hook
  (magit-mode . magit-delta-mode)
  :custom
  (magit-delta-default-dark-theme "Solarized (dark)")
  ;; (magit-delta-default-dark-theme "gruvbox-dark")
  ;; (magit-delta-default-dark-theme "base16")
  ;; (magit-delta-default-dark-theme "Nord")
  (magit-delta-hide-plus-minus-markers nil)
  ;; :config
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
  )

(use-package git-timemachine)

(use-package hl-todo
  :custom
  (hl-todo-wrap-movement t)
  (hl-todo-require-punctuation t)
  (hl-todo-keyword-faces
   '(("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("MAYBE" . "#7cb8bb")
     ("QUESTION" . "#7cb8bb")
     ("WHY" . "#7cb8bb")
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

;; Dependency of `org-mode'.
(use-package ob-async)

;; `org-babel' support for evaluating go code.
(use-package ob-go)

(use-package org
  :config
  (require 'ox-md)
  (defun org-mode-setup ()
    "Run after `org-mode' is initiated."
    (org-indent-mode)
    (when (member "Sarasa Mono SC Nerd" (font-family-list))
      (set-face-attribute 'org-table nil :font (font-spec :name "Sarasa Mono SC Nerd" :size 16))
      (if (display-graphic-p) (set-fontset-font t nil "Sarasa Mono SC Nerd" nil 'append)))
    (setq-local corfu-auto-delay 0.2))

  (defun individual-visibility-source-blocks ()
    "Fold blocks with header arguments `:hidden' when opening a org-mode buffer."
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

  (defun my/advise-org-edit-src-code (f &rest args)
    "Temporarily make current window undedicated if needed."
    (let ((dedicated-p (window-dedicated-p)))
      (if dedicated-p
          (progn
            (my/toggle-window-dedication)
            (apply f args)
            (my/toggle-window-dedication)))))

  ;; set `org-src-window-setup' to 'current-window
  ;; this will not work, tinker later
  ;; (advice-add 'org-edit-src-code :around 'my/advise-org-edit-src-code)

  (defun my/org-babel-tangle-rename ()
    (let ((tangle-file (buffer-file-name)))
      (rename-file tangle-file my/org-babel-tangle-dir t)))

  (defun my/org-edit-src-code-advice (oldfun &rest args)
    "Call OLDFUN with ARGS, then set a jump point with `better-jumper-set-jump'."
    (apply oldfun args)
    (setq-local completion-at-point-functions (remove 'cape-dabbrev completion-at-point-functions)))

  ;; Refactor to make it work in org indirect buffer.
  (defun org-babel-result-to-file (result &optional description type)
    "Convert RESULT into an Org link with optional DESCRIPTION.
If the `default-directory' is different from the containing
file's directory then expand relative links.

If the optional TYPE is passed as `attachment' and the path is a
descendant of the DEFAULT-DIRECTORY, the generated link will be
specified as an an \"attachment:\" style link."
    (when (stringp result)
      (let* ((result-file-name (expand-file-name result))
             (base-file-name (buffer-file-name (buffer-base-buffer)))
             ;; begin_modify
             (base-directory (file-name-directory base-file-name))
             ;; end_mordify
             (same-directory?
	            (and base-file-name
	                 (not (string= (expand-file-name default-directory)
			                           (expand-file-name
			                            base-directory)))))
             (request-attachment (eq type 'attachment))
             (attach-dir (let* ((default-directory base-directory)
                                (dir (org-attach-dir nil t)))
                           (when dir
                             (expand-file-name dir))))
             (in-attach-dir (and request-attachment
                                 attach-dir
                                 (string-prefix-p
                                  attach-dir
                                  result-file-name))))
        (format "[[%s:%s]%s]"
                (pcase type
                  ((and 'attachment (guard in-attach-dir)) "attachment")
                  (_ "file"))
                (if (and request-attachment in-attach-dir)
                    (file-relative-name
                     result-file-name
                     (file-name-as-directory attach-dir))
	                (if (and default-directory
		                       base-file-name same-directory?)
		                  (if (eq org-link-file-path-type 'adaptive)
		                      (file-relative-name
		                       result-file-name
                           (file-name-directory
			                      base-file-name))
		                    result-file-name)
		                result))
	              (if description (concat "[" description "]") "")))))

  :config
  ;; `cape-dabbrev' slows Emacs down
  (advice-add 'org-edit-src-code :around 'my/org-edit-src-code-advice)

  ;; NOTE: Enable it when we want to tangle a file, edit the file with lsp enabled,
  ;; and detangle it back to the org source block.
  (add-hook 'org-babel-post-tangle-hook 'my/org-babel-tangle-rename)
  ;; (remove-hook 'org-babel-post-tangle-hook 'my/org-babel-tangle-rename)

  ;; NOTE: Use absolute links to make `org-babel-detangle' work when the filename of
  ;; the tangled file is transformed in `org-babel-post-tangle-hook', namely
  ;; `my/org-babel-tangle-dir'.
  (setq org-babel-tangle-use-relative-file-links nil)

  :config
  ;; Make sure org-indent face is available


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
  (org-tags-column -85)
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
  (org-ellipsis ((t (:foreground "#E0CF9F" :underline nil))))
  (org-tag ((t (:weight normal))))

  ;;; For `doom-one'.
  ;; (org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3 :width normal :family "Iosevka"))))
  ;; (org-level-2 ((t (:inherit outline-2 :extend nil :height 1.2 :width normal :family "Iosevka"))))
  ;; (org-level-3 ((t (:inherit outline-3 :extend nil :height 1.1 :width normal :weight normal :family "Iosevka"))))
  ;; (org-level-4 ((t (:inherit outline-4 :extend nil :height 1.05 :width normal :weight normal :family "Iosevka"))))
  ;; (org-level-5 ((t (:inherit outline-5 :extend nil :height 1.0 :width normal :weight normal :family "Iosevka"))))
  ;; (org-level-6 ((t (:inherit outline-6 :extend nil :height 1.0 :width normal :weight normal :family "Iosevka"))))
  ;; ;; (org-block ((t (:inherit nil :extend t :background "#282c34")))) ;; the original: "#23272e"
  ;; (org-block-begin-line ((t (:inherit org-block :extend t :foreground "#83898d")))) ;; the original: "#5B6268"
  ;; ;; (org-block ((t (:background "#23272e" :extend t))))
  ;; (org-checkbox-statistics-todo ((t (:inherit org-todo :family "Iosevka"))))
  ;; (org-code ((t (:inherit nil :foreground "#da8548"))))
  ;; (org-verbatim ((t (:foreground "#98be65"))))
  ;; (org-document-title ((t (:foreground "#c678dd" :weight bold :height 1.2 :family "Iosevka"))))
  ;; (org-link ((t (:inherit link :foreground "#51afef" :family "Iosevka"))))
  ;; (org-table ((t (:foreground "#a9a1e1" :slant normal :weight normal :height 180 :width normal :foundry "nil"
  ;;                             :family "Sarasa Mono SC Nerd"))))

  ;;; For `solarized-gruvbox-dark'.
  ;; (org-level-1 ((t (:inherit variable-pitch :extend nil :foreground "#d65d0e" :height 1.3 :family "Iosevka"))))
  ;; (org-level-2 ((t (:inherit variable-pitch :extend nil :foreground "#98971a" :height 1.2 :family "Iosevka"))))
  ;; (org-level-3 ((t (:inherit variable-pitch :extend nil :foreground "#458588" :height 1.1 :family "Iosevka"))))
  ;; (org-level-4 ((t (:inherit variable-pitch :extend nil :foreground "#d79921" :height 1.05 :family "Iosevka"))))
  ;; (org-level-5 ((t (:inherit variable-pitch :extend nil :foreground "#689d6a" :family "Iosevka"))))
  ;; (org-level-6 ((t (:inherit variable-pitch :extend nil :foreground "#98971a" :family "Iosevka"))))
  ;; (org-link ((t (:inherit link :family "Iosevka"))))
  (org-link ((t (:inherit link :family "Iosevka")))))

(use-package org
  :after ob-go
  :config
  ;; org-agenda
  (defvar  my/GTD-file "~/Notes/Roam/20220816100518-gtd.org")
  (defvar my/org-babel-tangle-dir "~/dev/org-babel-tangle/")
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-babel-js-cmd "deno run")
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
     (go . t)
     ;; Rust support is provided by `rustic'.
     ;; (rust . t)
     ))
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
   ("M-s-<up>" . org-priority-up)
   ("M-s-<down>" . org-priority-down)
   ("M-i" . consult-org-heading)
   ("C-c C-x H-i" . org-clock-in)
   :map c-mode-base-map
   ("C-c C-l" . org-store-link)
   :repeat-map
   org-repeat-map
   ("-" . org-ctrl-c-minus)))

(use-package org-mac-image-paste
  ;; This package depends on `org-mode'. Use straight to install it will cause `org-mode' version
  ;; incompatibility. Setting `(straight-use-package 'org)' brings another problem that some packages are not
  ;; compatible with the latest `org-mode'.
  :load-path "site-lisp/org-mac-image-paste/"
  :config (org-mac-image-paste-mode 1)
  :bind (:map org-mode-map ("<f6>" . org-mac-image-paste-refresh-this-node)))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package org-sticky-header
  ;; Checkout wip/27-fix-fetch-stickyline due to issue:
  ;; https://github.com/alphapapa/org-sticky-header/issues/27 
  :load-path "site-lisp/org-sticky-header/"
  :hook
  (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-always-show-header nil)
  (org-sticky-header-full-path 'full))

(use-package ultra-scroll
  :if (eq window-system 'mac)
  :load-path "site-lisp/ultra-scroll/"
  ;; :init
  ;; (setq scroll-conservatively 101) ; important for jumbo images
  :config
  (ultra-scroll-mode 1)
  (add-hook 'pdf-view-mode-hook (defun disable-ultra-scroll-mac-mode-in-pdf-view ()
                                  (ultra-scroll-mode -1))))

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
  (org-bullets-bullet-list '("⊛" "◇" "✳" "◎" "○" "⊚" "●")))

(use-package org-download
  :after org
  :defer nil
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "~/Pictures/org-images")
  (org-download-heading-lvl nil)
  :bind
  ("C-s-y" . org-download-clipboard))

(use-package org-journal
  :preface
  (defun my/org-journal-new-todo (prefix)
    "Create a new todo entry in `org-journal'."
    (interactive "P")
    (org-journal-new-entry prefix)
    (org-todo))

  (defun my/org-journal-cycle-after-open-current-journal-file ()
    "Toggle visibility according to buffer's setting."
    (org-cycle t))

  (defun org-journal-file-header-func (_time)
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

  :init
  ;; Change default prefix key, needs to be set before loading
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/Journal/")
  (setq org-journal-file-type 'weekly)
  (advice-add 'org-journal-open-current-journal-file :after 'my/org-journal-cycle-after-open-current-journal-file)

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
   ("C-c j t" . my/org-journal-new-todo)
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
  (setq org-noter-notes-search-path "~/Books")
  (setq org-noter-highlight-selected-text t)
  :config
  (defun my/org-noter-with-org-roam-node ()
    "Create an `org-noter' session based on an `org-roam' node.
Integrate the workflow of opening a `org-roam' node and creating an `org-noter'
session on it without disturbing the current window configuration."
    (interactive)
    (make-frame '((name . "org-noter")))
    (select-frame-by-name "org-noter")
    (org-roam-node-find)
    (org-noter 0))
  :bind
  (("M-s-." . org-noter)
   ("<f7>" . my/org-noter-with-org-roam-node)
   :map org-noter-doc-mode-map
   ("i" . org-noter-insert-precise-note)
   ("M-i" . org-noter-insert-note)))

(use-package org-fragtog
  :disabled
  :hook (org-mode . org-fragtog-mode))

(use-package doct
  :init
  (setq my/roam-dir "~/Notes/Roam/"
        my/anki-file "~/Notes/Roam/anki-20220517104105.org"
        my/contacts-file "~/Notes/Roam/contacts-20220620203106.org"
        my/meeting-file (concat my/roam-dir "meeting-20221115143855.org")
        my/anecdote-file (concat my/roam-dir "anecdote-20240207175232.org")
        my/weekly-plan-file (concat my/roam-dir "weekly_plan-20240430174046.org"))
  (setq org-capture-templates
        (doct '(("Anki"
                 :keys "a"  ;; "a" for "Anki"
                 :file my/anki-file
                 :children
                 (("Basic card with a front and a back"
                   :keys "b"
                   :type plain
                   :headline "Dispatch Shelf"
                   :template ("** %^{The front}"
                              ":PROPERTIES:"
                              ":ANKI_NOTE_TYPE: Basic"
                              ":ANKI_DECK: %^{Deck|[1] English}"
                              ":END:"
                              "%x"))
                  ("Cloze"
                   :keys "c"
                   :headline "Dispatch Shelf"
                   :template ("** %<%Y-%m-%dT%H:%M:%S>"
                              ":PROPERTIES:"
                              ":ANKI_NOTE_TYPE: Cloze"
                              ":ANKI_DECK: %^{Deck|[1] English}"
                              ":END:"
                              "*** Text"
                              "%?"
                              "*** Back extra"
                              "%x"))
                  ("Temporary Note"
                   :keys "t"
                   :type plain
                   :headline "Temporary Notes"
                   :template ("** TODO %^{Topic|..}"
                              "%?"))))
                ("Contacts"
                 :keys "c"  ;; "c" for "Contacts"
                 :file my/contacts-file
                 :children
                 (("Family"
                   :keys "f"
                   :type plain
                   :headline "Family"
                   :template ("** %^{Name}"
                              ":PROPERTIES:"
                              ":CREATED: %<%Y-%m-%dT%H:%M:%S>"
                              ":BIRTHDAY: %^{BIRTHDAY}"
                              ":END:"
                              "%?"))
                  ("Friends"
                   :keys "F"
                   :type plain
                   :headling "Friends"
                   :template ("** %^{Name}"
                              ":PROPERTIES:"
                              ":CREATED: %<%Y-%m-%dT%H:%M:%S>"
                              ":HowDoWeMeet: %^{How do we meet?}"
                              ":END:"
                              "%?"))))
                ("Get Things Done"
                 :keys "g"  ;; "g" for "Get Things Done"
                 :file my/GTD-file
                 :children
                 (("Questions"
                   :keys "q"
                   :type plain
                   :regexp "\\* Questions \\[[0-9]*/[0-9]*\\]"
                   :template ("TODO %^{What is the QUESTION} %^g"
                              ":PROPERTIES:"
                              ":CREATED: %<%Y-%m-%dT%H:%M:%S>"
                              ":END:")
                   :immediate-finish nil
                   :after-finalize org-fold-hide-drawer-all)
                  ("Tasks"
                   :keys "t"
                   :type plain
                   :regexp "\\* Tasks \\[[0-9]*/[0-9]*\\]"
                   :template ("** TODO %^{What is the TASK} %^g"
                              ":PROPERTIES:"
                              ":ID: %(shell-command-to-string \"uuidgen | tr -d '\n'\")"
                              ":CREATED: %<%Y-%m-%dT%H:%M:%S>"
                              ":END:")
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
                  ("Later"
                   :keys "l"
                   :type plain
                   :regexp "\\* Reminders \\[[0-9]*/[0-9]*\\]"
                   :template ("** TODO %^{What is the REMINDER} %^g"
                              ":PROPERTIES:"
                              ":ID: %(shell-command-to-string \"uuidgen | tr -d '\n'\")"
                              ":CREATED: %<%Y-%m-%dT%H:%M:%S>"
                              ":END:")
                   :immediate-finish nil
                   :after-finalize org-fold-hide-drawer-all)))
                ("Weekly Plan"
                 :keys "w"
                 :file my/weekly-plan-file
                 :after-finalize org-fold-hide-drawer-all
                 :children
                 (("Weekdays"
                   :keys "d"
                   :type plain
                   :function (lambda ()
                               "Locate * Weekly Plan heading. Insert a timestamp of current week if there is none. create a TODO"
                               (goto-char (point-min))
                               (unless (re-search-forward
                                        (concat "\\* " (format-time-string "%Y-%m") "\\(-[0-9]*\\)?"
                                                (format-time-string "-W%V"))
                                        (point-max)
                                        t)
                                 (goto-char (point-max))
                                 (newline)
                                 (insert (concat "* " (format-time-string "%Y-%m-%d-W%V"))))
                               (unless (re-search-forward "\\*\\* Weekdays" (point-max) t)
                                 (newline)
                                 (insert "** Weekdays")))
                   :template ("*** TODO %^{What to do this week} %^g"
                              ":PROPERTIES:"
                              ":CREATED: %<%Y-%m-%dT%H:%M:%S>"
                              ":END:"))
                  ("Weekends"
                   :keys "e"
                   :type plain
                   :function (lambda ()
                               "Insert a timestamp of current week if there is none"
                               (goto-char (point-min))
                               (unless (re-search-forward
                                        (concat "\\* " (format-time-string "%Y-%m") "\\(-[0-9]*\\)?"
                                                (format-time-string "-W%V"))
                                        (point-max)
                                        t)
                                 (goto-char (point-max))
                                 (newline)
                                 (insert (concat "* " (format-time-string "%Y-%m-%d-W%V"))))
                               (unless (re-search-forward "\\*\\*\\* Weekends" (point-max) t)
                                 (newline)
                                 (insert "** Weekends")))
                   :template ("*** TODO %^{What to do this weekend} %^g"
                              ":PROPERTIES:"
                              ":CREATED: %<%Y-%m-%dT%H:%M:%S>"
                              ":END:"))))
                ("Meetings"
                 :keys "m"
                 :file my/meeting-file
                 :children
                 (("Random"
                   :keys "r"
                   :headline "Participated"
                   :template ("** %^{What is it about}  %^g"
                              ":PROPERTIES:"
                              ":ID: %(shell-command-to-string \"uuidgen | tr -d '\n'\")"
                              ":CREATED: %<%Y-%m-%dT%H:%M:%S>"
                              ":END:")
                   :immediate-finish nil
                   :jump-to-captured t
                   :after-finalize org-fold-hide-drawer-all)))
                ("Anecdote"
                 :keys "n"
                 :file my/anecdote-file
                 :type plain
                 :template ("* %^{What is it about} %^g"
                            ":PROPERTIES:"
                            ":CREATED: %<%Y-%m-%dT%H:%M:%S>"
                            ":END:"))))))

(use-package rime
  :custom
  (default-input-method "rime")
  ;; (rime-disable-predicates
  ;;  '(rime-predicate-prog-in-code-p
  ;;    rime-predicate-auto-english-p
  ;;    rime-predicate-punctuation-after-ascii-p
  ;;    rime-predicate-punctuation-line-begin-p
  ;;    rime-predicate-tex-math-or-command-p
  ;;    rime-predicate-org-latex-mode-p
  ;;    rime-predicate-current-uppercase-letter-p))
  (rime-cursor "˰")
  (rime-posframe-style 'vertical)
  (rime-show-candidate 'posframe)
  ;; :config
  ;; (define-key rime-active-mode-map (kbd "M-S-j") 'rime-inline-ascii)
  ;; (setq rime-translate-keybindings
  ;;       '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>"
  ;;         "<down>" "<prior>" "<next>" "<delete>" "C-`" "C-v" "M-v"))
  :bind
  (:map rime-mode-map
        ("M-s-r" . rime-force-enable)
        ("C-`" . rime-send-keybinding)))

;;; Architecture
;;; 1. epdfinfo runs poppler in server mode, receives request from emacs;
;;; 2. poppler renders pdf into PNGs on the go and return them to emacs;
;;; 3. poppler can provide rich information about the pdf and also the
;;; ability to modify it.
;; `pdf-tools' handles dependencies for us.
(use-package pdf-tools
  :if (display-graphic-p)
  :init
  (pdf-tools-install)
  :preface
  (defun my/pdf-tools-toggle-mouse-1-use ()
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
        ("C-<down-mouse-1>" . my/pdf-tools-toggle-mouse-1-use)
        ("[" . shrink-window-horizontally)
        ("]" . enlarge-window-horizontally)
        ("s" . isearch-forward)
        ("f" . image-forward-hscroll)
        ("b" . image-backward-hscroll)
        ("1" . pdf-annot-add-highlight-markup-annotation)
        ("2" . pdf-annot-add-underline-markup-annotation)
        ("3" . pdf-annot-add-squiggly-markup-annotation)
        ("4" . pdf-annot-delete)
        ("5" . pdf-annnt-add-text-annotation)
        ;; FIXME: On MacOS with emacs-29.1, a mouse wheel scroll by default invokes `mac-mwheel-scroll'. If
        ;; the height of the page (or more precisely the rendered image for a pdf's page) is greater than the
        ;; Emacs frame height, a mouse wheel scroll followed by a mouse click will cause the page to auto
        ;; scroll until its bottom meets the frame bottom.
        ([remap mac-mwheel-scroll] . mwheel-scroll))
  :custom
  ;; If scrolling cause noticeable delays, try setting it to nil
  (pdf-view-use-scaling t)
  (pdf-view-continuous t)
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1)
  ;; Select by word by default and use `my/pdf-tools-toggle-mouse-1-use' to toggle
  (pdf-view-selection-style 'word)
  (pdf-view-use-imagemagick t)
  (pdf-cache-prefetch-delay 0.1)
  (pdf-outline-display-buffer-action '(display-buffer-in-side-window
                                       (side . left)
                                       (window-width . 50))))

(use-package pdf-occur
  :after pdf-tools
  :ensure nil
  :bind
  (:map pdf-isearch-minor-mode-map
        ;; List lines matching STRING or PCRE.
        ("O" . pdf-occur)))

(use-package pdf-links
  :after pdf-tools
  :ensure nil
  :bind
  (:map pdf-links-minor-mode-map
        ("f" . image-forward-hscroll)
        ("S" . pdf-links-isearch-link)))

(use-package doc-toc
  :ensure nil
  :load-path "site-lisp/doc-tools-toc/")

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :bind
  (:map nov-mode-map
        ("n" . next-line)
        ("p" . previous-line)
        ("f" . forward-char)
        ("b" . backward-char)
        ("o" . nov-goto-toc)
        ("<return>" . nov-browse-url)
        :map nov-button-map
        ("n" . next-line)
        ("p" . previous-line))
  :config
  (add-hook 'nov-mode-hook (defun my/nov-mode-setup ()
                             (blink-cursor-mode -1))))

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
  (aw-leading-char-face ((t (:height 1.0 :weight bold :box (:line-width (2 . 2) :color "grey75" :style released-button) :foreground "red" :inherit aw-mode-line-face))))
  :custom
  (aw-char-position 'top-left))

(use-package zoom
  :custom
  (zoom-mode nil))

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file (concat user-emacs-directory "bookmarks"))
  ;; Column width of bookmark name in `bookmark-menu' buffer
  (bookmark-bmenu-file-column 80)
  (bookmark-use-annotations t)
  (bookmark-save-flag 1))

(use-package bookmark+
  :load-path "site-lisp/bookmark-plus/"
  :custom
  (bmkp-last-as-first-bookmark-file "~/.config/emacs/bookmarks")
  ;; TODO: new feature to auto light when the bookmark is annotated.
  (bmkp-auto-light-when-set nil)
  (bookmark-automatically-show-annotations nil)
  (bookmark-version-control t)
  (bmkp-add-bookmarks-here-menu-flag t)
  (bmkp-auto-light-when-set 'all-in-buffer)
  (bmkp-light-style-non-autonamed 'region)
  :custom-face
  (bmkp-local-file-without-region ((t (:foreground "#DCDCCC" :weight normal))))
  :bind
  ;; Useful commands
  ;; ("C-x j . f" . bmkp-file-this-dir-jump): jump to bookmarks under the current dir
  ;;
  ;; TODO: new pseudo bookmark type to jump to ones belonging to a given project.
  ;; Reference: One way to define such a command is to first mark the bookmarks that you want to be the
  ;; completion candidates, then use ‘C-c C-j’ (command ‘bmkp-bmenu-define-jump-marked-command’) in the
  ;; bookmark list.
  (("C-x x <DEL>" . bmkp-delete-bookmarks)
   ("C-x j c" . (lambda ()
                  "Most of the code is taken from `bmkp-jump-to-type' to get all bookmarks in the current buffer."
                  (interactive)
                  (let* ((type-cands                                  (bmkp-types-alist))
                         (icicle-unpropertize-completion-result-flag  t)
                         (type                                        "this-file")
                         (history                                     (assoc-default type type-cands))
                         (alist                                       (if history
                                                                          (funcall
                                                                           (intern (format "bmkp-%s-alist-only" type)))
                                                                        bookmark-alist))
                         (bmk                                         (bmkp-read-bookmark-for-type
                                                                       type alist nil
                                                                       ;; PREDICATE if not a recognized type name.
                                                                       (and (not history)
                                                                            (bmkp-handler-pred (intern type)))
                                                                       history)))
                    (bmkp-jump-to-type bmk))))
   ;; TODO: edit the bookmark under point
   :map bookmark-bmenu-mode-map
   ("M-o" . ace-window)
   ("M-n" . my/next-k-lines)
   ;; ("n" . (lambda ()
   ;;          (interactive)
   ;;          (next-logical-line)
   ;;          (bookmark-bmenu-switch-other-window)))
   ;; ("p" . (lambda ()
   ;;          (interactive)
   ;;          (previous-logical-line)
   ;;          (bookmark-bmenu-switch-other-window))))
   ("n" . next-line)
   ("p" . previous-line))
  :config
  (set-face-attribute 'bmkp-local-directory nil :background 'unspecified :weight 'normal)
  (set-face-attribute 'bmkp-local-file-without-region nil :foreground (face-foreground 'default) :weight 'normal)
  (add-to-list
   'display-buffer-alist
   '("\\*Bookmark Annotation Compose\\*" . ((display-buffer-in-side-window)
                                            (side . left)
                                            (slot . 1)
                                            (window-width . 45))))
  (add-hook 'find-file-hook (defun light-bookmarks ()
                              ;; Light all bookmarks in the current buffer.
                              (let ((bmk-alist (bmkp-this-buffer-alist-only)))
                                (bmkp-light-bookmarks bmk-alist)))))

;; Registers
(use-package register
  :ensure nil
  :custom
  (register-preview-delay 0.1)
  ;; Use register functions provided by `consult'.
  ;; :bind
  ;; ("s-x" . copy-to-register)
  ;; ("s-i" . insert-register)
  )


(defun my/increment-number-decimal (&optional arg)
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

(defun my/decrement-number-decimal (&optional arg)
  "Decrement the number forward from point by ARG."
  (interactive "p*")
  (my/increment-number-decimal (if arg (- arg) -1)))

(use-package fzf
  :load-path "site-lisp/fzf/"
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

(use-package super-save)

(use-package consult
  :preface
  (defun my/consult-imenu-thing-at-point (prompt items)
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

  (defun my/consult-ripgrep-thing-at-point (dir)
    "Search with `rg' for files in DIR with `thing-at-point' as initial input."
    (interactive "P")
    (consult--grep "Ripgrep" #'consult--ripgrep-make-builder dir (thing-at-point 'symbol t)))

  :custom
  ;; Maually and immidiate
  (consult-preview-key 'any)
  (consult-narrow-key "<")
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  :bind
  (("M-i" . consult-imenu)
   ("C-s-i" . (lambda ()
                (interactive)
                (my/consult-imenu-thing-at-point
                 "Go to item: "
                 (consult--slow-operation "Building Imenu..."
                   (consult-imenu--items)))))
   ("M-s-i" . consult-imenu-multi)
   ("s-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("C-M-y" . consult-yank-from-kill-ring)
   ("s-5" . consult-kmacro)
   ([remap bookmark-jump] . consult-bookmark)
   ("M-g f" . consult-flymake)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)                  ;; Alternative: consult-fd
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s s" . consult-ripgrep)
   ("M-s M-s" . my/consult-ripgrep-thing-at-point)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   ;; Register
   ("s-r" . consult-register-store)
   ("s-i" . consult-register-load)
   ("C-s-r" . consult-register)
   :map isearch-mode-map
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
   :map global-map
   ;; Visit previous marks on the current buffer
   ;; Original is `back-to-indentation' whose function has been integrated into `smarter-move-beginning-of-line'.
   ("M-s m" . consult-mark)
   ("M-s ." . (lambda ()
                (interactive)
                (consult-line (thing-at-point 'symbol))))
   ;; Remaps
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ([remap projectile-switch-to-buffer] . consult-project-buffer))

  :config
  (advice-add 'consult-bookmark :around (lambda (oldfun &rest args)
                                          (apply oldfun args)
                                          (recenter)))

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :custom-face
  (consult-file ((t (:inherit font-lock-constant-face))))
  ;; :config
  ;; reference: https://takeonrules.com/2024/02/28/configuring-consult-imenu-narrowing-for-ruby/
  ;; (add-to-list 'consult-imenu-config '(rust-mode
  ;;                                      :toplevel "Method"
  ;;                                      :types ((?M "Module" font-lock-type-face)
  ;;                                              (?m "Method" font-lock-function-name-face))))
  )

(use-package consult-projectile
  :bind
  ([remap projectile-find-file] . consult-projectile-find-file)
  ([remap projectile-find-file-other-window] . consult-projectile-find-file-other-window)
  ([remap projectile-find-file-other-frame] . consult-projectile-find-file-other-frame)
  ([remap projectile-switch-project] . consult-projectile-switch-project))

(use-package consult-todo)

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package go-mode
  :bind
  (:map go-mode-map
        ([remap beginning-of-defun] . go-goto-function)))

(use-package go-ts-mode
  :disabled
  :custom
  (go-ts-mode-indent-offset 2)
  :config
  (add-hook 'go-ts-mode-hook (defun set-treesit-font-lock-feature-list-for-go-ts-mode ()
                               (setq-local treesit-font-lock-feature-list
                                           '((comment definition)
                                             (keyword string type)
                                             (constant escape-sequence label number function property operator
                                                       error delimiter identifier
                                                       assignment builtin preprocessor variable)
                                             (bracket))))))

(use-package tramp
  :custom
  (tramp-default-proxies-alist nil)
  (tramp-verbose 3)
  (tramp-auto-save-directory "~/tmp/tramp/")
  (tramp-chunksize 2000))

(use-package treesit
  :disabled
  :ensure nil
  :config
  ;; When `treesit-extra-load-path' is nil, emacs looks in `tree-sitter' under `user-emacs-directory' to for
  ;; language grammars. We want this cos `treesit-install-language-grammar' installs grammar to the same dir.
  (setq treesit-extra-load-path nil)
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
          ;; The master branch is not compatible.
          ;; Ref: https://www.reddit.com/r/emacs/comments/1ahsg2n/trouble_with_treesitter_grammar_versioning
          ;; (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")))

	;; Reference: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  ;; (setq treesit-load-name-override-list '((c++ "libtree-sitter-c++" "tree_sitter_cpp")))

  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang))))

  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          ;; (js2-mode . js-ts-mode)
          ;; (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          ;; Until I figure out a more balanced `treesit-font-lock-level'.
          ;; (go-mode . go-ts-mode)
          ;; (go-mod-mode . go-mod-ts-mode)
          ;; (rust-mode . rust-ts-mode)
          ;; `c++-ts-mode' does not highlight symbols inside macros correctly.
          ;; (c++-mode . c++-ts-mode)
          )))

(use-package treesitter-context
  :after posframe-plus
  :load-path "site-lisp/treesitter-context/"
  :custom
  (treesitter-context-hide-frame-after-move t)
  (treesitter-context-idle-time 0.1))

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode)
  (rust-ts-mode . cargo-minor-mode))

;; (use-package rust-mode
;;   :custom
;;   (rust-indent-offset 4)
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.rs\\.~\\(.*\\)~" . rust-mode))
;;   (add-hook 'rust-mode-hook (defun my/rust-mode-hook ()
;;                               ;; TODO(zino): read `fill-column' from rustfmt.toml.
;;                               (setq-local fill-column 80))))

(use-package rustic
  :load-path "site-lisp/rustic/"
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(use-package js
  :config
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))
  :hook
  (js-mode . (lambda ()
               (setq-local format-all-formatters '(("JavaScript" deno)))))
  :bind
  (:map js-mode-map ("C-c C-p" . my/rustic-popup)))

(use-package typescript-ts-mode
  :mode ("\\.ts")
  :ensure nil)

(use-package toml-ts-mode
  :ensure nil
  :custom
  (toml-ts-mode-indent-offset 0))

(use-package protobuf-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.proto\\`" . protobuf-mode)))

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
  (lsp-intelephense-multi-root nil)
  ;; reset lsp session in lsp mode
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
  (add-to-list 'auto-mode-alist '("\\`README\\'" . markdown-mode))
  ;; `markdown-mode' is enabled to help rendering eldoc buffer content. `markdown-get-lang-mode' is used to
  ;; determine which language mode to use and `markdown-code-lang-modes' is consulted first. Customize this
  ;; variable to decide whether we want the treesitter version of lang mode.
  (add-to-list 'markdown-code-lang-modes '("go" . go-mode))
  :bind
  (:map markdown-mode-map
        ("M-n" . my/next-k-lines)
        ("M-p" . my/previous-k-lines)))

;; eglot add `eglot--post-self-insert-hook' to `post-self-insert-hook', which calls `eglot-format' and will
;; format the buffer acccording to the lsp servers' configuration. E.g. .clang-format for clangd.
(use-package eglot
  :load-path "site-lisp/eglot/"
  ;;; Performance tweaking
  :config
  ;; (fset #'jsonrpc--log-event #'ignore)
  ;; Tweak logging.
  (setf (plist-get eglot-events-buffer-config :size) 0)  
  ;; Inlay hints require `clangd-15' and is enabled by default, go get it!
  ;; Specify "--query-driver=/usr/bin/g++" to use stdlib.
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd" "--clang-tidy" "--header-insertion=iwyu" )))
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd" "--clang-tidy" "--header-insertion=iwyu")))
  (add-to-list 'eglot-server-programs '(cmake-mode . ("cmake-language-server")))
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode typescript-ts-mode) . (eglot-deno "deno" "lsp")))
  ;; lua-language-server's indent formatting respects `tab-width', and not .luarc.json. Not sure why.
  (add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(beancount-mode . ("beancount-language-server")))
  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
  ;; NOTE: If we also specify `go-ts-mode' to be served by `gopls', the builtin `go-ts-mode' will be loaded
  ;;  and it will add an entry ("\\.go\\'" . go-ts-mode) to `auto-mode-alist'. So don't specify `go-ts-mode'
  ;;  here if we don't want make `go-ts-mode' the default mode to serve go files.
  (add-to-list 'eglot-server-programs '((go-mode) . ("gopls" :initializationOptions
                                                     (:hints (
                                                              :parameterNames t
                                                              :rangeVariableTypes t
                                                              :functionTypeParameters t
                                                              :assignVariableTypes t
                                                              :compositeLiteralFields t
                                                              :compositeLiteralTypes t
                                                              :constantValues t)))))

  ;; (add-to-list 'eglot-server-programs '(conf-toml-mode . ("taplo")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list
     :enable t
     :lint t))
  :bind
  (:map eglot-mode-map
        ("M-." . xref-find-definitions)
        ("M-'" . eglot-find-typeDefinition)
        ("C-c d" . xref-find-definitions-other-window)
        ("C-c r"   . eglot-rename)
        ("C-c C-r" . xref-find-references)
        ("C-c H-i" . eglot-find-implementation)
        ("C-c C-a" . eglot-code-actions)
        ("C-c ." . display-local-help))
  :config
  :hook
  (rust-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
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
  (rustic-mode . eglot-ensure)
  ;; :custom-face
  ;; (eglot-highlight-symbol-face ((t (:foreground "#DFDFDF" :background "#34536c" :weight bold))))
  ;; (eglot-highlight-symbol-face ((t (:inherit bold))))
  ;; (eglot-inlay-hint-face ((t (:foreground "#5B6268" :height 1.0 :family "Iosevka"))))

  ;;; For `doom-one' theme.
  ;; (eglot-inlay-hint-face ((t (:foreground "#979797" :height 1.0 :family "Iosevka"))))
  ;; (eglot-parameter-hint-face ((t (:foreground "#979797" :height 1.0 :family "Iosevka"))))
  ;; (eglot-type-hint-face ((t (:foreground "#979797" :height 1.0 :family "Isoveka"))))

  :custom
  (eglot-sync-connect nil)
  ;; NOTE: Important. The default is nil, and will cause `xref-find-definitions'
  ;; to fail in external rust crates. (TODO: find out why it failed.)
  (eglot-extend-to-xref t)
  (eglot-autoshutdown nil)
  (eglot-send-changes-idle-time 0.2)
  :config
  ;; Stop `eglot' from turning on `flymake-mode'. Useful when `flycheck-mode' is intended for use.
  ;; (add-to-list 'eglot-stay-out-of 'flymake)
  )

(use-package eglot
  :config
  ;; https://github.com/joaotavora/eglot/issues/98
  (defun my/project-try-cargo-toml (dir)
    "Try to locate a Rust project above DIR."
    (let ((found (locate-dominating-file dir "Cargo.toml")))
      (if (stringp found) `(transient . ,found) nil)))

  ;; Try rust projects before version-control (vc) projects
  (add-hook 'project-find-functions 'my/project-try-cargo-toml nil nil)

  (defun my/eglot-hover-eldoc-function (cb)
    "Same as `eglot-hover-eldoc-function`, but throw away its short :echo cookie"
    (eglot-hover-eldoc-function (lambda (info &rest _ignore)
                                  ;; ignore the :echo cookie that eglot-hover-eldoc-function offers
                                  (funcall cb info))))
  (add-hook 'eglot-managed-mode-hook (defun my/remove-eglot-eldoc-func ()
                                       (if (and
                                            (bound-and-true-p tide-mode)
                                            (or (eq major-mode 'js-mode)
                                                (eq major-mode 'typescript-ts-mode)))
                                           (setq-local eldoc-documentation-functions
                                                       (delete 'eglot-hover-eldoc-function
                                                               eldoc-documentation-functions)))))
  (defun my/display-local-help-eldoc-function (_cb)
    ;; Return the string as doc. Return t if we need to run the provided callback manually.
    (help-at-pt-kbd-string))

  (add-hook
   'eglot-managed-mode-hook
   (defun my/configure-eldoc-documentation-functions ()
     ;; (setq eldoc-documentation-functions
     ;;       (cl-substitute #'my/eglot-hover-eldoc-function #'eglot-hover-eldoc-function
     ;;                      eldoc-documentation-functions))
     (add-hook 'eldoc-documentation-functions #'my/display-local-help-eldoc-function -90 t))))

(use-package eglot
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
  )

(use-package eglot
  :config
  (cl-defun eglot-imenu ()
    "Eglot's `imenu-create-index-function'.
Returns a list as described in docstring of `imenu--index-alist'."
    (unless (eglot-server-capable :documentSymbolProvider)
      (cl-return-from eglot-imenu))
    (let* ((res (eglot--request (eglot--current-server-or-lose)
                                :textDocument/documentSymbol
                                `(:textDocument
                                  ,(eglot--TextDocumentIdentifier))
                                :cancel-on-input non-essential))
           (head (and (cl-plusp (length res)) (elt res 0))))
      (message "res: %s, head: %s" res head)
      (when head
        (eglot--dcase head
          (((SymbolInformation)) (eglot--imenu-SymbolInformation res))
          (((DocumentSymbol)) (eglot--imenu-DocumentSymbol res))))))

  (defun eglot-imenu ()
    "Eglot's `imenu-create-index-function'.
Returns a list as described in docstring of `imenu--index-alist'."
    (cl-labels
        ((unfurl (obj)
           (eglot--dcase obj
             (((SymbolInformation)) (list obj))
             (((DocumentSymbol) name children)
              (cons obj
                    (mapcar
                     (lambda (c)
                       (plist-put
                        c :containerName
                        (let ((existing (plist-get c :containerName)))
                          (if existing (format "%s::%s" name existing)
                            name))))
                     (mapcan #'unfurl children)))))))
      (mapcar
       (pcase-lambda (`(,kind . ,objs))
         (cons
          (alist-get kind eglot--symbol-kind-names "Unknown")
          (mapcan (pcase-lambda (`(,container . ,objs))
                    (let ((elems (mapcar
                                  (lambda (obj)
                                    (cons (plist-get obj :name)
                                          (car (eglot--range-region
                                                (eglot--dcase obj
                                                  (((SymbolInformation) location)
                                                   (plist-get location :range))
                                                  (((DocumentSymbol) selectionRange)
                                                   selectionRange))))))
                                  objs)))
                      (if container (list (cons container elems)) elems)))
                  (seq-group-by
                   (lambda (e) (plist-get e :containerName)) objs))))
       (seq-group-by
        (lambda (obj) (plist-get obj :kind))
        (mapcan #'unfurl
                (jsonrpc-request (eglot--current-server-or-lose)
                                 :textDocument/documentSymbol
                                 `(:textDocument
                                   ,(eglot--TextDocumentIdentifier))
                                 :cancel-on-input non-essential)))))))

(use-package eglot-booster
  :load-path "site-lisp/eglot-booster/"
  :hook
  (eglot-managed-mode . eglot-booster-mode)
  :config
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp"))))

(use-package eglot-x
  :load-path "site-lisp/eglot-x/")

(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
   ;; Load breakpoints on startup
   (after-init . dape-breakpoint-load))

  :init
  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-on-start-hooks 'dape-info)
  ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root))

(use-package yasnippet
  :hook
  (after-init . yas-global-mode)
  :bind
  ("C-<tab>" . yas-expand))

(use-package yasnippet-snippets)

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("C-c [" . flymake-goto-prev-error)
        ("C-c ]" . flymake-goto-next-error)
        ("C-c C-e" . (lambda ()
                       "Open the window listing errors and switch to it."
                       (interactive)
                       (flymake-show-buffer-diagnostics)
                       (pop-to-buffer (flymake--diagnostics-buffer-name)))))
  (:repeat-map flymake-repeat-map
               ("[" . flymake-goto-prev-error)
               ("]" . flymake-goto-next-error))
  :hook
  (prog-mode . flymake-mode)
  :custom
  (help-at-pt-display-when-idle t)
  (next-error-function 'flymake-goto-next-error)
  (flymake-no-changes-timeout 0.1)
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-indicator-type 'fringes)
  (sh-shellcheck-program ""))

(use-package flymake-cursor
  :after flymake
  :config (flymake-cursor-mode 1)
  :bind
  ("s-." . flymake-cursor-show-errors-at-point-now))

(use-package flymake-clippy
  :disabled
  :after flymake
  :config
  ;; Instruct Eglot to stop managing Flymake.
  (add-to-list 'eglot-stay-out-of 'flymake)
  ;; Manually re-enable Eglot's Flymake backend
  (defun manually-activate-flymake ()
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
    (flymake-mode 1))
  :hook
  (rust-mode . flymake-clippy-setup-backend)
  (eglot-managed-mode . manually-activate-flymake))

(use-package flymake-popon
  :after flymake)

(use-package flycheck
  ;; Does not provide diagnostics while editing in `rust-mode'.
  :disabled
  :config
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
  (defun my/flycheck-next-error-advice (oldfun &rest args)
    (apply oldfun args)
    (recenter))

  :hook
  (prog-mode . flycheck-mode)

  ;; Don't integrate flycheck's error messages and eglot's documentation for
  ;; now. Use `flycheck-display-error-messages'.
  ;; (eglot-managed-mode . mp-flycheck-prefer-eldoc)

  ;; NOTE: `rustic-clippy' really slows emacs down.
  ;; :config
  ;; (push 'rustic-clippy flycheck-checkers)
  :custom
  (flycheck-display-errors-delay 0.6)
  (flycheck-indication-mode 'left-fringe)
  (flycheck-go-golint-executable "go-staticcheck")
  (flycheck-clang-language-standard "c++17")
  (flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (flycheck-error-list-format [("File" 9)
                               ("Line" 4 flycheck-error-list-entry-< :right-align t)
                               ("Col" 3 nil :right-align t)
                               ("Level" 7 flycheck-error-list-entry-level-<)
                               ("ID" 16 t)
                               (#("Message (Checker)" 0 7
                                  (face flycheck-error-list-error-message)
                                  9 16
                                  (face flycheck-error-list-checker-name))
                                0 t)])
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
  (:repeat-map flycheck-mode-repeat-map
               ("[" . flycheck-previous-error)
               ("]" . flycheck-next-error))
  :config
  (advice-add 'flycheck-next-error :around 'my/flycheck-next-error-advice))

;; Show matching parenthesis
(use-package mic-paren
  ;; Use `show-paren-mode'.
  :disabled
  :custom-face
  (paren-face-match ((t (:foreground "#7bb6e2" :background "#2c3946" :weight ultra-bold))))
  ;; :config
  ;; (paren-activate)
  :custom
  (paren-display-message 'always))

(use-package button-lock)

(use-package anki-editor
  :load-path "site-lisp/anki-editor/"
  :hook
  (org-capture-after-finalize . my/anki-editor-reset-cloze-num)
  (org-capture . anki-editor-mode)
  :config
  (defun my/anki-editor-reset-cloze-num ()
    "Reset cloze number of current card to 1."
    (interactive)
    (setq anki-editor-cur-cloze-num 1))

  (defun my/anki-editor-cloze-region-auto-incr ()
    "Cloze active region or word under corsor without hint.
  Automatically increase cloze number"
    (interactive)
    (anki-editor-cloze-dwim anki-editor-cur-cloze-num "")
    (sp-forward-sexp)
    (setq anki-editor-cur-cloze-num (1+ anki-editor-cur-cloze-num)))

  (defun my/anki-editor-cloze-region-not-incr ()
    "Cloze active region or word under corsor without hint.
  Do not increase cloze number"
    (interactive)
    (anki-editor-cloze-dwim anki-editor-cur-cloze-num "")
    (sp-forward-sexp))

  (defun my/anki-editor-push-tree ()
    "Push cards of the org subtree under cursor to Anki."
    (interactive)
    (anki-editor-push-notes '(4))
    (my/anki-editor-reset-cloze-num))

  (defun my/anki-editor-push-buffer ()
    "Push cards of this buffer."
    (interactive)
    (anki-editor-push-notes)
    (my/anki-editor-reset-cloze-num))

  :config
  ;; initialize cloze states
  (my/anki-editor-reset-cloze-num)
  (setq anki-editor-create-decks t
        anki-editor-org-tags-as-anki-tags t)
  :bind
  ("C-c m i" . anki-editor-insert-note)
  ("C-c m r" . my/anki-editor-reset-cloze-num)
  ("C-c m c" . my/anki-editor-cloze-region-auto-incr)
  ("C-c m p" . my/anki-editor-push-tree)
  ("C-c m b" . my/anki-editor-push-buffer))

(use-package ankiorg
  :load-path "site-lisp/ankiorg/")

(use-package font-lock
  :ensure nil
  :custom-face
  ;;; Tweak for `doom-one' theme.
  ;; (font-lock-comment-face ((t (:foreground "#83898d")))) ;; the original: ;; "#5B6268"
  ;; (font-lock-doc-face ((t (:family "Fira Code" :foreground "#83898d" :inherit font-lock-comment-face))))
  ;; (font-lock-doc-face ((t (:family "Iosevka" :foreground "#7cb8bb"))));; "#7F9F7F"))));;"#9FC59F")))) ;;"#8CA276")))) ;; the original: "#83898d"

  ;;; Tweak for `doom-gruvbox' theme.
  ;; (font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "#83898d"))))
  )

(use-package winner
  :hook
  (after-init . winner-mode)
  :bind
  (:map winner-repeat-map
        ("<left>" . winner-undo)
        ("<right>" . winner-redo)))

;; lua mode
(use-package lua-mode
  :bind
  (:map lua-mode-map
        ([remap beginning-of-defun] . lua-beginning-of-proc)
        ([remap end-of-defun] . lua-end-of-proc))
  :custom
  (lua-indent-level 4)
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(use-package perl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.t$" . perl-mode)))

(use-package make-mode
  :config
  (add-to-list 'auto-mode-alist '(".*Makefile" . makefile-gmake-mode)))

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

(use-package tide
  :config
  (add-hook 'typescript-ts-mode-hook (defun setup-tide-mode ()
                                       (tide-setup)
                                       (tide-hl-identifier-mode)))
  (add-hook 'js-mode-hook (defun setup-tide-mode ()
                            (tide-setup)
                            (tide-hl-identifier-mode)))
  :custom-face
  (tide-hl-identifier-face ((t (:inherit eglot-highlight-symbol-face))))
  :custom
  (tide-always-show-documentation t)
  (tide-completion-show-source t)
  (tide-hl-identifier-idle-time 0.1)
  (tide-imenu-flatten nil)
  ;; For large files the `imenu-create-index-function' `tide-imenu-index' may receive a bloated response from tsserver.
  (tide-server-max-response-length 1024000)
  (tide-completion-detailed t)
  :config
  (advice-add 'tide-project-root :around 'my/silence-advice))

(use-package dabbrev
  :bind
  (("M-/" . dabbrev-expand))
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
  :load-path "site-lisp/corfu/"
  :custom
  (corfu-cycle t)                      ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                       ;; Enable auto completion
  (corfu-separator ?\s)                ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)  ;; Quit at separator
  (corfu-quit-no-match t)              ;; Quit if there is no match
  (corfu-preview-current 'insert)      ;; Disable current candidate preview
  (corfu-preselect 'first)             ;; Preselect the prompt
  (corfu-on-exact-match 'show)         ;; Configure handling of exact matches
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
  (corfu-echo-delay '(0.2 . 0))

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("RET" . corfu-insert))

  ;; This is recommended since Dabbrev can be used globally (M-/s ).
  ;; See also `corfu-exclude-modes'.
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :custom-face
  (corfu-echo ((t (:inherit corfu-default))))
  ;; (corfu-bar ((t (:background "#a8a8a8" :weight bold))))
  ;; (corfu-border ((t (:weight bold :width extra-expanded))))
  ;; (corfu-current ((t (:background "#2c3946" :foreground "#bbc2cf"))))
  ;; :config
  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  ;; (setq text-mode-ispell-word-completion nil)
  :config
  (add-hook 'js-mode-hook (defun set-corfu-delay-for-js-mode ()
                            (setq-local corfu-auto-delay 0.2))))

(use-package corfu
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer))

;; Add extensions for `completion-at-point-functions'
(use-package cape
  :load-path "site-lisp/cape"
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
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :custom
  (cape-dabbrev-min-length 6))

(use-package yasnippet-capf
  :disabled
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  (setq yasnippet-capf-lookup-by 'name) ;; Prefer the name of the snippet instead
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless flex basic partial-completion emacs22)
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
  ;; after entry to minibuffer, hence is not suitable to set completion
  ;; styles as it cannot differentiate `eval-expression' from `M-x'.
  (eval-expression-minibuffer-setup . (lambda ()
                                        (setq-local completion-styles '(orderless partial-completion basic))))
  :config
  (add-hook 'minibuffer-mode-hook (defun completion-styles-for-minibuffer ()
                                    "Enable autocompletion on files."
                                    (add-to-list 'completion-at-point-functions 'cape-file)
                                    (setq-local completion-styles '(orderless flex basic
                                                                              partial-completion))))
  :custom-face
  ;;; Tweak for `solarized-gruvbox-dark'
  ;; (orderless-match-face-0 ((t (:foreground "#458588" :weight bold :background "#2c3946"))))
  ;; (orderless-match-face-1 ((t (:foreground "#d3869b" :weight bold :background "#373344"))))
  ;; (orderless-match-face-2 ((t (:foreground "#d79921" :weight bold :background "#333a38"))))
  ;; (orderless-match-face-3 ((t (:foreground "#98971a" :weight bold :background "#3b3a3b"))))
  )

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  ;; To enable for completion UI's with margin-formatters capability such as corfu:
  (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
  (setq kind-icon-default-style 
        ;; Make height smaller to fully show corfu candidates.
        ;; Reference: https://github.com/jdtsmith/kind-icon/issues/40
        '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0 :background nil))
  (setq kind-icon-blend-background nil))

(use-package lsp-bridge
  :disabled
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
  :if (not window-system)
  :ensure t
  :init
  (global-clipetty-mode))

;; git-modes
(use-package git-modes)

;; Customize `rg-menu' to pass additional flags to rg:
;; https://emacs.stackexchange.com/questions/74040/using-rg-ripgrep-passing-files-without-match-option-flag
(use-package rg
  :if (eq 0 (shell-command "which rg"))
  :custom
  ;; (rg-executable (executable-find "rg"))
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
  (comint-prompt-read-only t)
  ;; :config
  ;; (setq comint-dynamic-complete-functions (delete 'comint-c-a-p-replace-by-expanded-history
  ;;                                                 comint-dynamic-complete-functions))
  )

(use-package format-all
  ;; bug: make bookmarks and org-remark notes invalid.
  :disabled
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
     ("TypeScript" deno)
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
  (defun my/better-jumper-advice (oldfun &rest args)
    "Call OLDFUN with ARGS, then set a jump point with `better-jumper-set-jump'."
    (let ((old-pos (point)))
      (better-jumper-set-jump old-pos)
      (apply oldfun args)
      ;; (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point)))) 1)
      ;;   (better-jumper-set-jump old-pos))
      ))

  :init
  :custom
  (better-jumper-context 'window)
  :bind
  ("C-o" . better-jumper-jump-backward)
  :config
  (global-set-key (kbd "H-i") 'better-jumper-jump-forward)

  :config
  (better-jumper-mode 1)
  (dolist (fn (list
               'xref-find-definitions
               'my/switch-other-buffer
               'helm-imenu
               'widget-button-press
               'org-return
               'beginning-of-buffer
               'end-of-buffer
               'c-beginning-of-defun
               'c-end-of-defun
               'lua-beginning-of-proc
               'lua-end-of-proc
               'org-open-at-point
               'org-open-at-mouse
               'counsel-bookmark
               'mark-whole-buffer
               'beginning-of-defun
               'end-of-defun
               'org-roam-node-find
               'my/find-user-init-file
               'avy-goto-char-timer
               'helm-maybe-exit-minibuffer
               'consult-imenu
               'consult-line
               'symbols-outline-move-depth-up
               'flymake-goto-prev-error
               'flymake-goto-next-error
               'sp-up-sexp))
    (advice-add fn :around 'my/better-jumper-advice)))

(use-package dogears
  :init (dogears-mode 1)
  :bind
  (:map global-map
        ("M-g d" . dogears-go)
        ("M-g M-b" . dogears-back)
        ("M-g M-f" . dogears-forward)
        ("M-g M-d" . dogears-list)
        ("M-g M-D" . dogears-sidebar)))

;; (use-package org-remark
;;   ;; Previous version: 1.0.5
;;   :init
;;   ;; Turn on `org-remark' highlights on startup
;;   (org-remark-global-tracking-mode +1)

;;   (defun my/org-remark-notes-file-name-function ()
;;     "Customize notes file."
;;     (let ((notes-dir (concat "~/Documents/org-remark" (file-name-directory (buffer-file-name)))))
;;       (make-directory notes-dir t)
;;       (concat notes-dir (file-name-base (buffer-file-name)) "-notes.org")))

;;   :config
;;   (defun my/org-remark-mark-and-open ()
;;     "Helper function to mark region and open notes buffer.
;;   I find myself often do this workflow."
;;     (interactive)
;;     (org-remark-mark (region-beginning) (region-end))
;;     (org-remark-open (1- (point))))

;;   (defun my/org-remark-open-advice (point &optional view-only)
;;     "Display source code edit buffer in `other-window' cause a `side-window'
;;   cannot be splitted."
;;     (setq-local org-src-window-setup 'other-window))

;;   (defun my/org-remark-eldoc-function (cb)
;;     ;; Return the string. Return t if we need to run the provided callback manually.
;;     (help-at-pt-kbd-string))

;;   :config
;;   (advice-add 'org-remark-open :after 'my/org-remark-open-advice)
;;   (advice-add 'org-remark-highlights-get :around 'my/silence-advice)
;;   (add-hook 'org-remark-mode-hook (lambda ()
;;                                     (define-key org-remark-mode-map [remap display-local-help] 'eldoc-print-current-symbol-info)))

;;   :bind
;;   ;; (keyboard-translate ?\C-m ?\H-m)
;;   (("C-x C-n m" . org-remark-mark)
;;    ("C-x C-n o" . org-remark-open)
;;    ("C-x C-n v" . org-remark-view)
;;    ("C-x C-n ]" . org-remark-view-next)
;;    ("C-x C-n [" . org-remark-view-prev)
;;    ("C-x C-n r" . org-remark-remove)
;;    ("C-x C-n d" . org-remark-delete)
;;    ("C-x C-n y" . org-remark-mark-yellow)
;;    ("C-x C-n l" . org-remark-mark-red-line)
;;    ("C-x C-n e" . my/org-remark-mark-and-open)
;;    ("C-x C-n t" . org-remark-toggle))
;;   (:repeat-map org-remark-repeat-map
;;                ("[" . org-remark-view-prev)
;;                ("]" . org-remark-view-next))
;;   :custom
;;   (org-remark-notes-display-buffer-action
;;    '((display-buffer-in-side-window)
;;      (side . left)
;;      (slot . 1)
;;      (window-width . 45)))
;;   (org-remark-notes-file-name 'my/org-remark-notes-file-name-function)
;;   (org-remark-icon-notes "")
;;   (org-remark-icon-position-adjusted "(m)"))

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
  (tab-bar-history-mode)
  ;; it is not common to mis-type this
  (global-unset-key (kbd "C-<tab>"))
  :bind
  ("M-<tab>" . tab-next)
  ("M-S-<tab>" . tab-previous)
  ("C-s-<tab>" . tab-bar-select-tab-by-name)
  ([remap winner-undo] . tab-bar-history-back)
  ([remap winner-redo] . tab-bar-history-forward)
  (:repeat-map tab-bar-history-repeat-map
               ("<left>" . tab-bar-history-back)
               ("<right>" . tab-bar-history-forward))
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-show nil)
  (tab-width 2)
  (tab-bar-history-limit 400))

;;; Language support
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

;; Major mode for SaltStack
(use-package salt-mode
  :custom-face
  (mmm-default-submode-face ((t nil))))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package isearch
  ;; Useful commands
  ;; ("M-s ." . isearch-forward-symbol-at-point)
  ;; ("M-s M-." . isearch-forward-thing-at-point)
  :preface
  (defun my/isearch-region-or-forward ()
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
  ("C-s-s" . my/isearch-region-or-forward)
  (:repeat-map isearch-repeat-map
               ("s" . isearch-repeat-forward)
               ("r" . isearch-repeat-backward))
  ;; :hook
  ;; Recenter after the match is selected, not recenter on every match.
  ;; It is still a little distracting :(
  ;; (isearch-mode-end . recenter-top-bottom)
  :config
  (add-hook 'post-command-hook
            (defalias 'my/repeat-change-cursor ; recolor cursor during repeat
              (let (repeat-p (ccol (face-background 'cursor)))
		            (lambda ()
                  (unless (eq repeat-p repeat-in-progress)
                    (setq repeat-p repeat-in-progress)
                    (set-cursor-color
		                 (if repeat-in-progress
			                   (face-foreground 'success)
		                   ccol))))))
            90))

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
     "http://nullprogram.com/feed/"
     "https://planet.emacslife.com/atom.xml"
     "https://fasterthanli.me/index.xml")))

(use-package cc-mode
  :ensure nil
  :config
  (defconst my/cc-style
    '("cc-mode"
      (c-offsets-alist . ((innamespace . [0])))))
  (c-add-style "cc-mode" my/cc-style)
  :bind
  (:map c-mode-base-map
        ([remap c-indent-line-or-region] . indent-for-tab-command)))

(use-package org-present
  :init
  (defun my/org-present-on-start ()
    (visual-fill-column-mode 1)
    (setq-local visual-fill-column-width 120)
    (setq-local org-hide-emphasis-markers t)
    ;; (face-remap-add-relative 'org-block-begin-line '(:background "#282c34"))
    ;; (face-remap-add-relative 'org-block-end-line '(:background "#282c34"))
    )

  (defun my/org-present-on-quit ()
    (visual-fill-column-mode -1)
    (setq-local org-hide-emphasis-markers nil)
    ;; (face-remap-add-relative 'org-block-begin-line '(:foreground "#83898d" :background "#23272e"))
    ;; (face-remap-add-relative 'org-block-end-line '(:foreground "#83898d" :background "#23272e"))
    )

  (defun my/org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines.
    (org-overview)
    ;; Unfold the current entry.
    (org-fold-show-entry)
    ;; Show only direct subheadings of the slide but don't expand them
    (org-fold-show-children))
  :hook
  (org-present-mode . my/org-present-on-start)
  (org-present-mode-quit . my/org-present-on-quit)
  :config
  (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)
  :custom
  (visual-fill-column-center-text t))

(use-package visual-fill-column)

(use-package god-mode
  :bind
  (:map god-local-mode-map
        ("C-w" . forward-to-word)
        ("C-." . xref-find-definitions)
        ("C-i" . better-jumper-jump-forward))
  :custom
  (god-mode-enable-function-key-translation nil))

;; Deal with terminal escape characters correctly in compilation buffer.
(use-package ansi-color
  :ensure nil
  :config
  (defun my/ansi-colorize-region (&optional beg end _len)
    (interactive)
    (let ((beg (or beg (point-min)))
          (end (or end (point-max)))
          (buffer-read-only nil))
      (ansi-color-apply-on-region beg end t)))
  (defun my/text-mode-and-ansi-colorize-buffer ()
    (text-mode)
    (my/ansi-colorize-region (point-min) (point-max) 0)
    (setq-local after-change-functions (add-to-list 'after-change-functions 'my/ansi-colorize-region)))
  (add-to-list 'auto-mode-alist '("\\.log\\(\\.[0-9]*-[0-9]*-[0-9]*\\)?" . my/text-mode-and-ansi-colorize-buffer))
  :hook
  (compilation-filter . my/ansi-colorize-region))

(use-package xterm-color
  :disabled
  :custom
  (xterm-color-render t))

(use-package avy
  :bind
  ("s-l" . avy-goto-line)
  ("C-s-l" . avy-goto-end-of-line)
  ("M-j" . avy-goto-char-timer)
  :custom
  (avy-background nil))

(use-package avy-zap
  :config
  (defun my/zap-up-to-char (arg char &optional interactive)
    "Customized `zap-up-to-char' to support reading a double quote from the
mibuffer. The only difference is this function uses `read-char' instead of
`read-char-from-minibuffer'.

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
  ("C-z" . my/zap-up-to-char))

(use-package crux
  :preface
  (defun my/find-user-init-file (other-window)
    "Open `user-init-file' in the current or other window based on OTHER-WINDOW."
    (interactive "P")
    (let ((init-file (or user-init-file "~/.config/emacs/init.el")))
      (if current-prefix-arg
          (find-file-other-window init-file)
        (find-file init-file))))

  :bind
  ("C-s-o" . crux-smart-open-line-above)
  ("s-d" . crux-duplicate-current-line-or-region)
  ("C-<backspace>" . crux-kill-line-backwards)
  ;; ("s-r" . crux-recentf-find-file)
  ("C-c I" . my/find-user-init-file)
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
  ;; (delete 'recenter-top-bottom pulsar-pulse-functions)
  )

(use-package xref
  :ensure nil
  :bind
  ;; ("C-." . xref-find-definitions)
  ;; ("C-," . xref-go-back)
  ("C-c M-d" . xref-find-definitions-other-frame)
  ("C-c C-r" . xref-find-references)
  :custom
  (xref-history-storage 'xref-window-local-history)
  (xref-search-program 'ripgrep)
  (xref-auto-jump-to-first-xref nil)
  :custom-face
  (xref-file-header ((t (:inherit orderless-match-face-0))))
  :config
  ;;; `zoom-mode' does not work well with side window. Comment below if using `zoom-mode'.
  (add-to-list
   'display-buffer-alist
   '("^\\*xref\\*"
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
  :config
  (remove-hook 'xref-after-return-hook 'xref-pulse-momentarily))

(use-package dumb-jump
  ;; Does not work with emacs 29.1
  :disabled
  :config
  (add-hook 'prog-mode-hook (defun activate-dumb-jump ()
                              (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)))
  :custom
  (dumb-jump-prefer-searcher 'rg))

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

(use-package eat
  :disabled)

(use-package vterm
  :bind
  (:map vterm-mode-map
        ("s-<return>" . vterm-send-return)
        ("C-<return>" . vterm-send-return)
        ("C-s-t" . vterm-copy-mode)
        ("C-/" . vterm-undo)
        ("M-j" . avy-goto-char-timer))
  (:map vterm-copy-mode-map
        ("q" . vterm-copy-mode)
        ("C-s-t" . vterm-copy-mode))
  :custom
  (vterm-timer-delay 0.01)
  :config
  (add-hook 'vterm-mode-hook (defun disable-global-hl-line-mode ()
                               (setq-local global-hl-line-mode nil)))
  (add-hook 'vterm-copy-mode-hook (defun enable-hl-line-mode ()
                                    (if vterm-copy-mode
                                        (hl-line-mode)
                                      (hl-line-mode -1)))))

(use-package vterm-toggle
  :config
  (defun my/vterm-toggle(&optional args)
    "Vterm toggle.
Optional argument ARGS ."
    (interactive "P")
    (cond
     ((or (derived-mode-p 'vterm-mode)
          (and (vterm-toggle--get-window)
               vterm-toggle-hide-method))
      (if (equal (prefix-numeric-value args) 1)
          (if (derived-mode-p 'vterm-mode)
              (vterm-toggle-hide)
            ;; A vterm window exists but is not focused, simply switch to it.
            (vterm-toggle-show))
        ;; Create a new vterm buffer window and switch to it.
        (vterm (format "%s<%s>" vterm-buffer-name (tab-bar-tab-name-current)))))
     ((equal (prefix-numeric-value args) 1)
      (vterm-toggle-show))
     ((equal (prefix-numeric-value args) 4)
      (let ((vterm-toggle-fullscreen-p
             (not vterm-toggle-fullscreen-p)))
        (vterm-toggle-show)))))

  (defun vterm-toggle--new(&optional buffer-name)
    "Bespoke version to add tab name into vterm buffer name.
New vterm buffer."
    (let* ((default-directory default-directory)
           ;; begin_patch
           (tabs (funcall tab-bar-tabs-function))
           (tab-index (tab-bar--current-tab-index tabs))
           (current-tab (nth tab-index tabs))
           (tab-name (alist-get 'name current-tab))
           (buffer-name (or buffer-name (format "%s<%s>" vterm-buffer-name tab-name)))
           ;; end_patch
           project-root)
      (when (and vterm-toggle-project-root
                 (eq vterm-toggle-scope 'project))
        (setq project-root (vterm-toggle--project-root))
        (when project-root
          (setq default-directory project-root)))
      (if vterm-toggle-fullscreen-p
          (vterm buffer-name)
        (if (eq major-mode 'vterm-mode)
            (let ((display-buffer-alist nil))
              (vterm buffer-name))
          (vterm-other-window buffer-name)))))

  :bind
  ("s-e" . my/vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  ;; (add-to-list 'display-buffer-alist
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

(use-package compile
  :custom
  ;; Auto scroll the compilation buffer to the end of output.
  (compilation-scroll-output t))
(use-package fish-mode)

(use-package nyan-mode
  :config
  (nyan-mode -1)
  :custom
  (nyan-cat-face-number 1))

(use-package screenshot
  :disabled
  :custom
  (screenshot-line-numbers-p t))

(use-package explain-pause-mode
  :load-path "site-lisp/explain-pause-mode/")

(use-package breadcrumb
  ;; :init
  ;; (setq-default frame-title-format
  ;;               '((:eval (breadcrumb-project-crumbs))
  ;;                 (:eval (and imenu--index-alist
  ;;                             (concat "  ◊  " (breadcrumb-imenu-crumbs))))))
  :custom
  (breadcrumb-project-max-length 0)
  (breadcrumb-imenu-max-length 80))

(use-package window-stool
  :disabled
  :load-path "site-lisp/window-stool/"
  :hook
  (prog-mode . window-stool-mode))

;; This will mess up `org-ellipsis', don't use it for now.
(use-package pp-c-l
  :disabled
  :ensure nil
  ;; :custom
  ;; (pp^L-^L-string "                                            ")
  :config
  (pretty-control-l-mode))

(use-package prism
  :disabled)

(use-package tla-mode
  :load-path "site-lisp/tla-mode/")

(use-package tla-tools
  :load-path "site-lisp/tla-tools/")

(use-package separedit
  :bind
  ("C-c C-'" . separedit))

(use-package org-ql)

(use-package beancount
  :init
  (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
  :commands beancount-mode
  :bind
  (:map beancount-mode-map
        ("<tab>" . indent-for-tab-command)))

(use-package plantuml-mode
  :if (eq 0 (shell-command "which plantuml"))
  :custom
  (plantuml-executable-path "plantuml")
  (plantuml-default-exec-mode 'executable)
  (org-plantuml-jar-path (let* ((executable (file-chase-links (executable-find "plantuml")))
                                (dir (file-name-parent-directory (file-name-parent-directory executable))))
                           (car (directory-files-recursively dir ".*\.jar"))))
  (plantuml-indent-level 2)
  :init
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode)))

(use-package gud
  :ensure nil
  :bind
  (:map gud-mode-map
        ;; `gud-mode' inherits `comint-mode' and `g' is bind to `recompile' in `comint-mode-map'.
        ("g" . self-insert-command)))

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
  :load-path "site-lisp/peek"
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
  :disabled)

(use-package org-valign
  :disabled)

(use-package org-modern
  :disabled)

(use-package indent-bars
  ;; Freezed Old version: commit #fb1a0d6
  :hook
  (hack-local-variables . (lambda ()
                            ;; Read `indent-bars-space-override' from
                            ;; `.dir-locals.el' first.
                            (when (or (derived-mode-p 'rust-mode)
                                      (derived-mode-p 'rust-ts-mode))
                              (indent-bars-mode))))
  (prog-mode . indent-bars-mode)
  (salt-mode . indent-bars-mode)
  (yaml-mode . indent-bars-mode)
  ;; (emacs-lisp-mode . (lambda ()
  ;;                      (indent-bars-mode -1)))
  (git-timemachine-mode . indent-bars-mode)
  :custom
  (indent-bars-color-by-depth
   '(:palette
     (outline-1 outline-2 outline-3 outline-4 outline-5 outline-6 outline-7)
     :blend 1))
  (indent-bars-pattern ". . . . ")
  (indent-bars-width-frac 0.2)
  (indent-bars-treesit-support t)
  (indent-bars-display-on-blank-lines t)
  (indent-bars-depth-update-delay 0.1)
  :hook
  ;; HACK: deal with uncorrectly displayed indent-bars in org mode source block.
  (org-mode . (lambda ()
                (let ((color (face-background 'org-block))
                      (num 30))
                  (cl-loop for i from 1 to num
                           for face = (intern (format "indent-bars-%d" i))
                           do
                           (face-remap-add-relative face `(:foreground ,color)))))))

(use-package embark
  :config
  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))
  (define-key embark-general-map (kbd "o") (my/embark-ace-action projectile-find-file))
  (define-key embark-general-map (kbd "n") (my/embark-ace-action org-roam-node-find))
  (define-key embark-buffer-map (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-buffer-map (kbd "o") (my/embark-ace-action consult-ripgrep))
  :bind
  ("s-o" . embark-act))

(use-package embark-consult)

(use-package visual-regexp
  :bind
  ([remap query-replace-regexp] . vr/query-replace))

(use-package posframe-plus
  :load-path "site-lisp/posframe-plus/")

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
  (zoom-window-mode-line-color "black")
  (zoom-window-mode-line-color "#3c586f"))

(use-package simple
  :ensure nil
  :bind
  ("C-/" . undo-only)
  ("M-_" . undo-redo)
  :custom
  (undo-no-redo t)
  :custom-face
  (next-error ((t (:inherit (bold region))))))

(use-package undo-hl
  ;; Too distracting
  :disabled
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

(use-package rebox2
  :load-path "site-lisp/rebox2/"
  :bind
  ("C-:" . rebox-cycle)
  :custom
  (rebox-style-loop '(23 25)))

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
  :load-path "site-lisp"
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

(use-package emacs
  :ensure nil
  :config
  (add-hook 'after-init-hook (defun my/set-split-window-configs ()
                               "Note frame width could change during loading `init.el'."
                               ;; At most two windows horizontally.
                               (interactive)
                               (setq split-width-threshold (1+ (/ (frame-width) 2)))
                               (setq split-height-threshold (window-height (selected-window)))
                               (setq split-window-preferred-function 'my/split-window-sensibly)))
  :config
  (defun my/split-window-sensibly (&optional window)
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
	               (split-window-below))))))))
  :bind
  ("C-x =" . balance-windows)
  ("C-x +" . what-cursor-position))

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
  (ediff-combination-pattern '("<<<<<<< variant A" A ">>>>>>> variant B" B "####### Ancestor" Ancestor
                               "======= end"))
  :config
  (defun ediff-copy-both-to-C ()
    "Swap buffer A and B with `~' before running this command if needed."
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map))

(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill))

(use-package re-builder
  :ensure nil
  :bind
  ("C-c R" . re-builder)
  (:repeat-map re-builder-repeat-map
               ("s" . reb-next-match)
               ("r" . reb-prev-match))
  :custom
  (reb-blink-delay 0.2)
  (reb-re-syntax 'string))

(use-package auto-dim-other-buffers)

(use-package key-chord
  :init (key-chord-mode 1)
  :config
  (key-chord-define-global ";'" "\C-e;")
  (key-chord-define-global "r4" 'consult-register)
  :custom
  (key-chord-two-keys-delay 0.02))

(use-package multiple-cursors
  ;; TODO: disable `repeat-mode' temporarily when using multiple cursors
  :bind
  ("C-s-n" . mc/mark-next-like-this-symbol)
  ("C-s-p" . mc/mark-previous-like-this-symbol)
  ("C-s-c C-s-c" . mc/edit-lines)
  ("C-s-v C-s-v" . mc/mark-all-dwim)
  :custom
  (mc/always-run-for-all t))

(use-package iedit
  :bind
  (("C-'"  . iedit-mode) ; also note: C-' shows/hides just matches
   :map iedit-mode-keymap
   ("C-g" . iedit-mode) ; so I can exit iedit with C-g
   ("C-x C-s" . (lambda ()
                  (interactive)
                  (save-buffer)
                  (iedit-mode))))
  :config
  (advice-add #'iedit--get-scope    ; switch default to function scope
	            :filter-args
	            (defun my/iedit-defun-by-default (arg)
		            (cond ((eq (car arg) nil) '(0))
		                  ((eq (car arg) 0) '(nil))
		                  (t arg)))))

(use-package subword
  :bind
  ("M-s-<left>" . subword-left)
  ("M-s-<right>" . subword-right))

(use-package string-inflection
  :config
  (defun my/string-inflection-pascal-case ()
    "FooBar format."
    (interactive)
    (string-inflection--single-or-region #'string-inflection-pascal-case-function))
  (defalias 'my/string-inflection-snake-case 'string-inflection-underscore)
  (defalias 'my/string-inflection-screaming-snake-case 'string-inflection-upcase)
  (defalias 'my/string-inflection-camal-case 'string-inflection-lower-camelcase)
  :bind
  ("C-c C-c p" . my/string-inflection-pascal-case)
  ("C-c C-c s" . my/string-inflection-snake-case)
  ("C-c C-c S" . my/string-inflection-screaming-snake-case)
  ("C-c C-c c". my/string-inflection-camal-case)
  ("C-c C-c k" . string-inflection-kebab-case))

(use-package iscroll
  :hook
  ;; Smoothly scroll over inline-images.
  (org-mode . iscroll-mode))

(use-package goto-chg
  :bind
  ("M-m" . goto-last-change)
  ("M-S-m" . goto-last-change-reverse))

(use-package ledger-mode
  :mode ("\\.ledger\\'")
  :custom
  (ledger-post-account-alignment-column 2))

(use-package persistent-scratch)

(use-package activities
  :config
  (activities-mode)
  (activities-tabs-mode)
  :bind
  (("C-, C-x C-n" . activities-new)
   ("C-, C-x C-d" . activities-define)
   ("C-, C-x C-r" . activities-resume)
   ("C-, C-x C-s" . activities-suspend)
   ("C-, C-x C-k" . activities-kill)
   ("C-, C-x RET" . activities-switch)
   ("C-, C-x b" . activities-switch-buffer)
   ("C-, C-x d" . activities-discard)
   ("C-, C-x g" . activities-revert)
   ("C-, C-x l" . activities-list)))

(use-package annotate
  :init
  (annotate-mode)
  :custom
  (annotate-use-echo-area t)
  (annotate-use-messages nil)
  :bind
  (("C-, C-a" . annotate-annotate)
   ("C-, C-d" . annotate-delete-annotation)
   ("C-, C-p" . annotate-change-annotation-text-position)
   ("C-, C-c" . annotate-change-annotation-colors)
   ("C-, C-s" . annotate-show-annotation-summary)
   ("C-, ]"   . annotate-goto-next-annotation)
   ("C-, ["   . annotate-goto-previous-annotation))
  :config
  (add-hook 'find-file-hook 'annotate-mode))

(use-package rust-playground
  :bind
  ("C-, C-r" . rust-playground)
  :custom
  (rust-playground-basedir "~/dev/rust-playground")
  (rust-playground-main-rs-template "fn main() {\n    \n    println!(\"Results:\");\n}"))

(use-package gptel
  :config
  (setq gptel-backend
        (gptel-make-openai "K"     ; Any name you want
          :host "api.siliconflow.cn"
          :endpoint "/v1/chat/completions"
          :stream t
          :key (getenv "OPENAI_API_KEY")
          :models '(Pro/deepseek-ai/DeepSeek-R1)))
  (global-set-key (kbd "C-c g") #'gptel)
  (global-set-key (kbd "C-c RET") #'gptel-send))

(use-package aidermacs
  :load-path "site-lisp/aidermacs/"
  :config
  (global-set-key (kbd "C-c l") 'aidermacs-transient-menu)
  (aidermacs-setup-minor-mode)
  ;; slilicon
  ;; (setq aidermacs-default-model "openai/Pro/deepseek-ai/DeepSeek-V3")
  ;; aliyun idealab
  (setq aidermacs-default-model "openai/qwen2.5-max")
  ;; (setq aidermacs-default-model "openai/DeepSeek-R1-671B")
  (setq aidermacs-default-model "openai/gpt-41-0414-global")
  (add-hook 'aidermacs-before-run-backend-hook
            (defun my/configure-aider ()
              (let* (;; Place to store input and chat history.
                     (chat-history-dir "~/.aider/chat-history")
                     (input-history-file-name ".aider.input.history")
                     (chat-history-file-name ".aider.chat.history.md")
                     (current-file-name (buffer-file-name))
                     (git-dir (vc-root-dir))
                     ;; Determine the project root and concatenate it after `chat-history-dir'.
                     (root-dir (or
                                ;; If the directory is version controlled, use the vc root dir.
                                (and git-dir (expand-file-name git-dir))
                                ;; Otherwise look for the nearest dir named "src". This often happens when
                                ;; visiting different vendors's source code located under /usr/local.
                                (locate-dominating-file current-file-name "src")
                                ;; The current directory is the default.
                                (expand-file-name (file-name-directory current-file-name))))
                     (des-dir (file-name-concat chat-history-dir root-dir))
                     (chat-history-path (expand-file-name chat-history-file-name des-dir))
                     (input-history-path (expand-file-name input-history-file-name des-dir)))
                (unless (file-directory-p des-dir)
                  (make-directory des-dir t))
                (setq-local aidermacs-extra-args
                            `("--restore-chat-history"
                              "--no-show-model-warnings"
                              "--chat-history-file" ,chat-history-path
                              "--input-history-file" ,input-history-path)))))

  (setq aidermacs-default-chat-mode 'architect)
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-vterm-multiline-newline-key "S-<return>")
  (setq aidermacs-show-diff-after-change nil))

(use-package page-break-lines
  :init (global-page-break-lines-mode))

(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-face ((t nil)))
  (pulse-highlight-start-face ((t nil))))

(use-package faces
  :ensure nil
  :custom-face
  (variable-pitch ((t (:weight regular :height 180 :family "Fira Code"))))
  (variable-pitch-text ((t (:inherit variable-pitch :height 1.0))))
  (help-key-binding ((t (:inherit fixed-pitch :background "grey19" :foreground "LightBlue" :box (:line-width (-1 . -1) :color "grey35") :height 120)))))

(use-package popper
  :load-path "site-lisp/popper/")

(use-package plz
  :load-path "site-lisp/plz")

(use-package minuet
  :load-path "site-lisp/minuet-ai.el/"
  :after plz
  :bind
  (("M-m" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
   ;; ("M-" . #'minuet-show-suggestion) ;; use overlay for completion
   ;; ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion)))

;; Try it some time.
;; (use-package sideline)
;; (use-package imenu-everywhere)
;; (use-package visual-regexp-steroids)
;; (use-package emacs-color-theme-solarized)
;; (use-package restclient)  ;; Test HTTP REST webservices
;; (use-package burly)
;; (use-package dired+)  ;; different colors for different file permissions
;; (use-package org-ioslide) ;; presentation
;; (use-package popwin)
;; (use-package imenu-list)
;; (use-package git-gutter+)
;; (use-package flycheck-inline)
;; (use-package forge)
;; (use-package lasgun)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages '((aider :url "git@github.com:tninja/aider.el.git"))))

;; Set at the end of init.el when `load-path' is ready.
(setq elisp-flymake-byte-compile-load-path load-path)

;; Run as daemon
(server-start)

;;; init.el ends here
