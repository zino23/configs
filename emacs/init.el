;; init.el --- Startup file for emacs -*- lexical-binding: t -*-

(setq load-prefer-newer t)

;; take the full control, don't load `default.el'
(setq inhibit-default-init t)

(require 'package)

;; I don't know why this is not loaded
(require 'ox)

;; Initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(setq user-emacs-directory "~/.config/emacs/")

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq confirm-kill-emacs #'yes-or-no-p)

(set-default-coding-systems 'utf-8)
(setq inhibit-startup-screen t)
(size-indication-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode nil)
(set-fringe-mode 7)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode -1)
(setq help-window-select t)
(setq desktop-auto-save-timeout 60)
(desktop-save-mode 1)

(setq save-silently t)

;; Keep buffers up to date
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Save minibuffer's history inputs
(setq history-length 25)
(savehist-mode t)
(recentf-mode t)

;; Delete files into trashbin
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash")

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Define a read-only directory class
(dir-locals-set-class-variables 'read-only '((nil . ((buffer-read-only . t)))))

;; Emacs source for help system
(setq emacs-src-dir "~/Dev/emacs-27.2")
(setq macos-sdk-dir "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX11.1.sdk")
(setq go-src-dir "/usr/local/Cellar/go/1.17.8/libexec/src")

;; Associate directories with the read-only class
(dolist (dir (list
              "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1"
              emacs-src-dir
              go-src-dir
              ))
  (dir-locals-set-directory-class (file-truename dir) 'read-only))

;; Rebind bc by default `list-buffers' list buffers in another window
(global-set-key "\C-x\C-b" 'ibuffer)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; keyboard scroll one line at a time
(setq scroll-step 1)
;; Disable the visible bell
(setq visible-bell nil)

;; Auto soft-break line
(auto-fill-mode nil)
(global-visual-line-mode t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Word abbreviation
(setq-default abbrev-mode t)
(setq abbrev-file-name "~/.config/emacs/.abbrev_defs")
(setq save-abbrevs 'silently)
(quietly-read-abbrev-file)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(use-package dired
  :ensure nil
  :commands
  (dired dired-jump)
  :bind
  (("C-x C-j" . dired-jump)))

(setenv "PATH" (concat "/Library/TeX/texbin" (getenv "PATH")))
(setq exec-path (append '("/Library/TeX/texbin") exec-path))

(defun copy-full-path-to-kill-ring ()
  "Copy buffer's full path to kill ring."
  (interactive)
  (when buffer-file-name (kill-new (file-truename buffer-file-name))
        (message buffer-file-name)))

;; Full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set up fonts for different charsets and make sure one Chinese char is as twice wide as one English char
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

(zino/set-font "Fira Code" "Sarasa Mono SC Nerd" 21 1)

;; check if font exist
;; (member "Sarasa Mono SC Nerd" (font-family-list))

;; Automatically trim white spaces at the end of line
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

;; Indicate indent level
(use-package highlight-indent-guides
  :hook
  (conf-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  (lispy-data-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (lispy-data-mode . rainbow-delimiters-mode)
  (debugger-mode . rainbow-delimiters-mode))

;; Display keybindings in another buffer
(use-package command-log-mode
  :config
  (global-command-log-mode -1)
  (global-set-key (kbd "C-c C-o") #'clm/toggle-command-log-buffer))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil
                      :background "#363e4a")
  (show-paren-mode 1)
  (setq show-paren-delay 0))

(use-package all-the-icons
  :if
  (display-graphic-p))

(use-package all-the-icons-dired
  :if
  (display-graphic-p)
  :hook
  (dired-mode . all-the-icons-dired-mode)
  (dired-mode . auto-revert-mode)
  :custom
  (auto-revert-verbose nil))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon nil)

(use-package emojify
  :hook
  (erc-mode . emojify-mode)
  :commands
  emojify-mode)

(use-package all-the-icons-dired)

(use-package undo-tree
  :init
  (global-undo-tree-mode t)
  :custom
  (undo-tree-auto-save-history nil))

(use-package smartparens
  :bind
  (("C-s-a" . sp-beginning-of-sexp)
   ("C-s-e" . sp-end-of-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-a" . sp-backward-down-sexp)
   ("C-M-e" . sp-up-sexp)
   ;; in the same nested level
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ;; unwrap
   ("M-D"   . sp-splice-sexp)
   ("C-M-r" . sp-rewrap-sexp)

   ;; copy
   ("C-M-y" . sp-copy-sexp))
  :config
  (smartparens-global-mode)

  ;; not smartparens related, but shares the same philosophy
  (global-set-key (kbd "C-s-f") #'beginning-of-defun)
  (global-set-key (kbd "C-s-g") #'end-of-defun))

;; Handle apostrophe and single quote in Lisp mode
(require 'smartparens-config)

(global-set-key (kbd "C-c (")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
(global-set-key (kbd "C-c [")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "[")))
(global-set-key (kbd "C-c {")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{")))
(global-set-key (kbd "C-c \"")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
(global-set-key (kbd "C-c \'")  (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\'")))
;; unwrap the next sexp; `sp-splice-sexp' unwrap the current
(global-set-key (kbd "C-c u") (lambda (&optional arg) (interactive "P") (sp-unwrap-sexp)))

(sp-pair "(" ")" :wrap "C-(")
(sp-pair "{" "}" :wrap "C-{")

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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-respect-visual-line-mode nil)
  (setq evil-cross-lines t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-move-cursor-back nil)
  (setq evil-auto-indent t)
  (setq evil-undo-system 'undo-tree)
  :config
  ;; (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package hydra)

(defhydra hydra-text-scale
  (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished"
   :exit t))

(use-package ivy
  :init (setq ivy-re-builders-alist '(;; (swiper . ivy--regex-plus)
                                      (t . ivy--regex-fuzzy)))
  :diminish
  :bind
  (("s-s" . swiper)
   ;; ("C-c f" . counsel-fzf)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   :map
   ivy-switch-buffer-map
   ("C-d" . ivy-switch-buffer-kill)
   :map
   ivy-reverse-i-search-map
   ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1)
  (define-key ivy-minibuffer-map [remap ivy-restrict-to-matches] (lambda () (interactive))))

(define-key minibuffer-mode-map (kbd "S-SPC") nil)

(setq ivy-use-selectable-prompt t)

(use-package which-key
  :init
  (which-key-mode)
  :diminish
  which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-show-docstrings nil))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   :map
   minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (setq ivy-prescient-enable-filtering t)
  :config
  ;; Remember candidate frequencies across sessions
  (ivy-prescient-mode 1)
  (setq ivy-prescient-retain-classic-highlighting t)
  ;; Not explicitly set this will not use ivy's fuzzy matching by default
  ;; Also this will not affect swiper's regex-plus method
  (setq prescient-filter-method '(literal regexp fuzzy)))

(prescient-persist-mode 1)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-define-key
   :prefix
   "C-c n"
   "c" 'org-roam-capture
   "f" 'org-roam-node-find
   "i" 'org-roam-node-insert
   "g" 'org-roam-graph)
  (general-define-key
   :prefix "C-c C-n"))

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  ;; (define-key projectile-mode-map (kbd "C-x C-f") #'project-find-file)
  :custom
  ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Dev")
    (setq projectile-project-search-path '("~/Dev")))
  (setq projectile-switch-project-action #'projectile-find-file))

;; (use-package counsel-projectile
;;  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-git-executable "/usr/local/bin/git")
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs < 27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; code review
(use-package forge)

;; git-gutter
(use-package git-gutter
  :bind
  (("C-x C-g" . git-gutter-mode)
   :repeat-map git-gutter-repeat-map
   ("[" . git-gutter:previous-hunk)
   ("]" . git-gutter:next-hunk))
  :config
  (general-define-key
   :prefix "<f5> g"
   "g" 'git-gutter
   "[" 'git-gutter:previous-hunk
   "]" 'git-gutter:next-hunk
   "p" 'git-gutter:popup-hunk
   "s" 'git-gutter:stage-hunk)
  :custom
  (git-gutter:update-interval 2 "Automatically update diff in 2 seconds")
  (git-gutter:window-width 1))

(defun org-mode-setup ()
  "Run after `org-mode' is initiated."
  ;; (setq evil-auto-indent nil)
  (org-indent-mode)
  (set-face-attribute 'org-table nil :font (font-spec :name "Sarasa Mono SC Nerd" :size 22))
  (set-fontset-font t nil "Sarasa Mono SC Nerd" nil 'append))

(use-package org
  :hook
  (org-mode . org-mode-setup)
  ;;(org-mode . valign-mode)
  ;; (org-insert-heading . evil-insert-state)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers nil
	    org-M-RET-may-split-line nil
	    org-list-allow-alphabetical t
	    org-return-follows-link t
	    org-log-into-drawer t
        ;; track todo changes
        ;; prepend with '/', when leaving this state and the target state does not specify @ or i
        ;; otherwise when entering
        ;; @: note with a timestamp
        ;; !: a timestamp
        ;; /!: a timestamp when *leaving* this state and the target state does not specify @ or i
	    org-todo-keywords '((sequence "TODO(t!)" "DOING(i@/!)" "QUESTION(n@/!)" "|" "DONE(d@)" "POSTPONED(p@)" "DELEGATED(g@)" "CANCELED(c@)"))
	    org-startup-folded t
	    org-cycle-include-plain-lists 'integrate
	    org-blank-before-new-entry (quote ((heading . nil)
					                       (plain-list-item . nil)))
	    org-edit-src-content-indentation 0
        org-catch-invisible-edits 'show-and-error
        org-goto-auto-isearch nil
        org-hide-block-startup t

        ;; issue with yasnippet when expanding snippet
        ;; https://www.reddit.com/r/emacs/comments/nj08dz/issues_with_yasnippet_in_emacs_272_lisp_error/
        org-src-tab-acts-natively nil

        ;; org-agenda
        zino/GTD-file "~/Notes/Roam/20220816100518-gtd.org"
        org-agenda-files '("~/Notes/Roam/Journal"))
  (push zino/GTD-file org-agenda-files)
  (define-key org-mode-map [remap org-return-and-maybe-indent] #'better-jumper-jump-forward)
  (define-key org-mode-map (kbd "C-S-j") #'org-return-and-maybe-indent)
  (setq org-image-actual-width nil)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  :custom
  (org-tags-column -80)
  (org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3 :width normal :family "Iosevka"))))
  (org-level-2 ((t (:inherit outline-2 :extend nil :height 1.2 :width normal :family "Iosevka"))))
  (org-level-3 ((t (:inherit outline-3 :extend nil :height 1.1 :width normal :family "Iosevka"))))
  (org-level-4 ((t (:inherit outline-4 :extend nil :height 1.05 :width normal :family "Iosevka"))))
  (org-level-5 ((t (:inherit outline-5 :extend nil :height 1.0 :width normal :family "Iosevka"))))
  (org-level-6 ((t (:inherit outline-6 :extend nil :height 1.0 :width normal :family "Iosevka"))))
  (org-block ((t (:inherit nil :extend t :background "#282c34"))))
  (org-block-begin-line ((t (:inherit all-faces :foreground "#5B6268"))))
  (org-checkbox-statistics-todo ((t (:inherit org-todo :family "Iosevka"))))
  (org-code ((t (:inherit nil :foreground "#da8548"))))
  (org-document-title ((t (:foreground "#c678dd" :weight bold :height 1.2 :family "Iosevka"))))
  (org-id-link-to-org-use-id 'create-if-interactive))

(use-package org-appear)

(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("⊛" "◎" "◉" "⊚" "○" "●")))

;; Make sure org-indent face is available
(require 'org-indent)

(use-package org-download
  :after org
  :defer nil
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "~/Pictures/org-images")
  (org-download-heading-lvl nil)
  (org-image-actual-width 300)
  :bind
  ("C-s-y" . org-download-clipboard)
  :config
  (require 'org-download))

;; Drag-and-drop to `dired'
(add-hook 'dired-mode-hook 'org-download-enable)

(defun org-roam-book-template ()
  "Create a Cornell-style book notes template for org-roam node.
Return TEMPLATE as a string. "
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

(use-package org-roam
  :custom
  (org-roam-directory "~/Notes/Roam")
  (org-roam-dailies-directory "Journal/")
  (org-roam-completion-everywhere t)
  ;;(org-roam-completion-system 'default)
  (org-roam-capture-templates
   '(("c" "Default" entry "* %?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${slug}\n#+FILETAGS: %^{tags}\n#+CREATED: %<%Y-%m-%d>\n#+STARTUP: folded")
      :empty-lines-before 1
      :unnarrowed nil)
     ;; ("b" "Book notes" plain (function org-roam-book-template)
     ;;  :target (file+head "Books/%^{genre}/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${slug}\n#+FILETAGS: %^{tags}\n#+CREATED: %<%Y-%m-%d>\n#+STARTUP: folded")
     ;;  :empty-lines-before 1
     ;;  :unnarrowed t)
     ;; ("o" "Outline" entry "* %?"
     ;;  ;; ID is created at heading level, olp is placed after heading
     ;;  :target (file+head+olp "%<%Y%m>.org" "* %t" ("%<%Y>" "%<%W>")))
     ;; ("d" "Day Tree" entry "* %?"
     ;;  :target (file+datetree "%<%Y%m%d>.org" day)
     ;;  :unnarrowed t)
     ;; ("w" "Week Tree" entry "* %?".org" week)
     ;;  :unnarrowed nil)
     ;; ("m" "Month Tree" entry "* %?"
     ;;  :target (file+datetree "%<%Y%m%d>.org" month)
     ;;  :unnarrowed nil)
     ;; ("t" "Temporary notes" entry "* %?"
     ;;  :target (file "Fleeting/%<%Y%m%d%H%M%S>-${slug}.org")
     ;;  :empty-lines-before 1
     ;;  :unnarrowed t)
     ))
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
  :config
  (org-roam-db-autosync-mode)
  ;; :hook
  ;; (org-capture-mode . evil-insert-state)
  )

(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
   (pcase org-journal-file-type
     (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
     (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
     (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
     (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
Saves the buffer of the current day's entry and kills the window.
Similar to `org-capture' like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(use-package org-journal
  :init
  ;; Change default prefix key, needs to be set before loading
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/Notes/Roam/Journal/")
  (setq org-journal-file-type 'weekly)
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
   :map org-journal-mode-map
   ("C-x s-s" . org-journal-save-entry-and-exit))
  :hook
  (org-journal-after-entry-create . org-narrow-to-element)
  ;; (org-journal-after-entry-create . evil-insert-state)
  )

;; (org-babel-load-file (concat user-emacs-directory "config.org"))

;;reset lsp session in lsp mode
;;(setq lsp--session nil)

;;(define-key c-mode-base-map (kbd "C-M-e") 'sp-up-sexp)
;;(define-key c-mode-base-map (kbd "C-M-a") 'sp-backward-down-sexp)

(require 'all-the-icons)

(global-set-key (kbd "C-x C-c") #'save-buffers-kill-terminal)

;; begin_config
(setq native-comp-async-report-warnings-errors nil)

;; (setq exec-path (cons "/usr/local/bin/python3" exec-path))

;; temporary workaround
;; REVIEW See Wilfred/elisp-refs#35 . Remove once fixed upstream.
(defvar read-symbol-positions-list nil)

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (progn
      (message "Native compilation is available"))
  (message "Native complation is *not* available"))
(setq native-comp-deferred-compilation t)

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
  :config
  (define-key rime-active-mode-map (kbd "M-S-j") 'rime-inline-ascii))

(setq rime-translate-keybindings
      '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>"
        "<down>" "<prior>" "<next>" "<delete>" "C-`" "C-v" "M-v"))

(global-set-key "\C-c\C-d" #'helpful-at-point)

(setq custom-file "~/.config/emacs/init.el")

(setq gc-cons-threshold (* 300 1024 1024))
;; end_config

;; begin_read
;;; arch
;; epdfinfo runs poppler in server mode, receives request from emacs
;; poppler render pdf into PNGs on the go and return them to emacs
;; poppler can provide rich information about the pdf and also the
;; ability to modify

;;; installation
;; poppler, epdfinfo
(when (not (package-installed-p 'pdf-tools))
  (package-install 'pdf-tools))
(pdf-tools-install)
(setq pdf-view-use-scaling t)
(define-key pdf-view-mode-map (kbd "n") #'pdf-view-next-line-or-next-page)
(define-key pdf-view-mode-map (kbd "p") #'pdf-view-previous-line-or-previous-page)
(define-key pdf-view-mode-map (kbd ",") #'pdf-view-previous-page)
(define-key pdf-view-mode-map (kbd ".") #'pdf-view-next-page)
(define-key pdf-view-mode-map (kbd "s-5") 'pdf-annot-add-highlight-markup-annotation)
(define-key pdf-view-mode-map (kbd "s-6") 'pdf-annot-add-squiggly-markup-annotation)
(define-key pdf-view-mode-map (kbd "s-7") 'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map (kbd "s-8") 'pdf-annot-delete)

;;(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
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
;; end_read

;; begin_UI
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

(setq indicate-buffer-boundaries t
      indicate-empty-lines nil)

(setq echo-keystrokes 0.01)

(setq scroll-margin 0
      scroll-preserve-screen-position t
      next-screen-context-lines 5)

;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "ETBembo" :height 250 :weight regular))))
;;  '(fixed-pitch ((t ( :family "Fira Code" :height 250)))))

(set-fontset-font
 t 'symbol
 (font-spec
  :family "Apple Color Emoji"
  :size 25
  :weight 'normal
  :width 'normal
  :slant 'normal))

(setq company-box-backends-colors '((company-yasnippet . (:all "#457b9d" :selected (:foreground "#1d3557" :background "#457b9d")))))

(use-package beacon
  :config
  (beacon-mode)
  :custom
  (beacon-blink-delay 0.2)
  (beacon-color "#a8dadc")
  (beacon-size 50))

(setq custom-safe-themes t)

(setq display-line-numbers-width 0)
(setq display-line-numbers-width-start 0)
;; end_UI

(global-set-key (kbd "C-x b") #'ido-switch-buffer)
(global-set-key (kbd "s-b") #'ido-switch-buffer)

;; begin_mastering_emacs
(setq apropos-sort-by-scores t)

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (global-set-key (kbd "<f5> k") 'ace-delete-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))

(setq next-screen-context-lines 2)

(blink-cursor-mode)
(setq blink-cursor-blinks 0)

;; bookmarks
(setq bookmark-default-file (concat user-emacs-directory "bookmarks"))

;; registers
(setq register-preview-delay 0.2)
;; end_mastering_emacs

(setq ivy-use-virtual-buffers nil)

;; begin_move_and_edit

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:50}" 'face 'org-tag)))

(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by ARG . "
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

(defun my-decrement-number-decimal (&optional arg)
  "Decrement the number forward from point by ARG."
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))

(global-set-key (kbd "C-c +") 'my-increment-number-decimal)

;;; begin_company_mode
(use-package company
  :bind
  (:map company-active-map
        ("<return>"   . company-complete-selection)
        ("<tab>"      . company-complete-common))
  ;; :hook
  ;; (emacs-lisp-mode . company-mode)
  ;; (org-mode        . company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.1)
  (company-require-match nil)
  :config
  (global-company-mode)
  ;;(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

(global-set-key (kbd "M-.") #'company-show-doc-buffer)
(defvar company-mode/enable-yas nil
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))


;;; end_company_mode
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
  (global-set-key (kbd "C-c f") #'fzf)
  (global-set-key (kbd "C-c d") #'fzf-directory))

(repeat-mode)

(define-key global-map [remap goto-line] 'avy-goto-line)

(pixel-scroll-mode 1)

;;; begin_autosave
;; autosave file-visiting buffer but not non-file-visiting e.g. *scratch*
(setq-default auto-save-default t)
(setq auto-save-timeout 15)
(setq auto-save-interval 100)
(setq-default auto-save-no-message t)

(setq
 auto-save-dir "~/.config/emacs/autosave/"
 auto-save-file-name-transforms
 `((" . *" "~/.config/emacs/autosave" t)))
;;; end_autosave

;;; begin_backup
(setq
 ;; turn on backup functionality
 make-backup-files t
 ;; also backup files that are version controlled
 vc-make-backup-files t
 ;; backup by renaming will make the existing file the backup,
 ;; and cause all links to link to the backup which probably is not what we wanted
 backup-by-copying t
 ;; control the naming of different versions ourselves
 version-control nil
 delete-old-versions t
 kept-new-versions 20
 kept-old-versions 20)
;;; end_backup

;; by default emacs create backup on first buffer save since the file is visited
;; C-u to save backup in second save (test above)
;; C-u C-u to immediately save into backup
(setq backup-directory-alist nil)

(defun make-backup-file-name (file)
  (let ((dirname (concat "~/.config/emacs/backup/"
                         (format-time-string "%Y-%m-%d/"))))
    (unless (file-exists-p dirname)
      (make-directory dirname t))
    (concat dirname (concat (replace-regexp-in-string "/" "!" (buffer-file-name)) (format-time-string "T%H:%M~")))))

;;; auto save current buffer
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))
;;;

;;; begin_ivy
(setq ivy-dynamic-exhibit-delay-ms 0)
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

(setq ivy-initial-inputs-alist '((counsel-minor . "^+")
                                 (counsel-package . "^+")
                                 (counsel-org-capture . "^")
                                 (counsel-M-x . "^")
                                 (counsel-describe-symbol . "^")
                                 (org-refile . "^")
                                 (org-agenda-refile . "^")
                                 (org-capture-refile . "^")
                                 (Man-completion-table . "^")
                                 (woman . "^")))

(defvar +ivy--queue-last-input nil)
(defun +ivy-queue-exhibit-a(f &rest args)
  (if (equal +ivy--queue-last-input (ivy--input))
      (ivy--exhibit)
    (apply f args))
  (setq +ivy--queue-last-input (ivy--input)))
(advice-add 'ivy--queue-exhibit :around #'+ivy-queue-exhibit-a)
;;; end_ivy

(defun find-file--auto-create-dir (filename &optional wildcards)
  "Create parent directory during visiting file if necessary .
Do not prompt me to create parent directory"
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(advice-add 'find-file :before 'find-file--auto-create-dir)

(delete-selection-mode 1)

(global-set-key (kbd "s-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "M-j") 'avy-goto-char-timer)

(sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))

;; speed up swiper
;; when visual-line mode is enabled, emacs will forward
;; by visual line which is very slow in large files
(setq swiper-use-visual-line nil)
(setq swiper-use-visual-line-p (lambda (a) nil))

(defun zino/swiper-isearch-again ()
  "Start swiper-isearch with the last thing searched for."
  (interactive)
  (swiper-isearch (car swiper-history)))
(global-set-key (kbd "s-F") 'zino/swiper-isearch-again)

(defun zino/delete-forward-char ()
  "Delete the char under cursor and keep it in the `kill-ring'."
  (interactive)
  (delete-forward-char 1 t))

(global-set-key (kbd "C-d") #'zino/delete-forward-char)

;; vim like newline
(defun newline-below ()
  "Newline like vim's o."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<C-return>") #'newline-below)

(defun newline-above ()
  "Newline like vim's O . "
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))
(global-set-key (kbd "C-S-<return>") #'newline-above)

(defun zino/next-k-lines ()
  "Move cursor down k lines"
  (interactive)
  (next-line 5))

(defun zino/previous-k-lines ()
  "Move cursor up k lines"
  (interactive)
  (next-line -5))

(global-set-key (kbd "M-n") #'zino/next-k-lines)
(global-set-key (kbd "M-p") #'zino/previous-k-lines)

(global-set-key (kbd "C-a") #'smarter-move-beginning-of-line)

(global-set-key (kbd "M-f") #'forward-to-word)
(global-set-key (kbd "M-e") #'forward-word)

(defun zino/kill-whole-line-without-newline ()
  "Kill the whole line but leave the trailing newline . "
  (interactive)
  (kill-whole-line 0))

(global-set-key (kbd "C-k") #'kill-whole-line)
(global-set-key (kbd "C-S-k") #'kill-line)
(global-set-key (kbd "C-S-s-k") #'zino/kill-whole-line-without-newline)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line        .
Move point to the first non-whitespace character on this line .
If point is already there, move to the beginning of the line  .
Effectively toggle between the first non-whitespace character and
the beginning of the line                                     .
If ARG is not nil or 1, move forward ARG - 1 lines first      . If
point reaches the beginning or end of the buffer, stop there  . "
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
      (beginning-of-visual-line))
    ))

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

(global-set-key (kbd "C-c k") #'copy-line)
(global-set-key (kbd "C-c C-k") #'copy-line)

;; Vim like return to last mark location
(global-set-key (kbd "C-o") #'pop-global-mark)

(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "M-Z") #'zap-to-char)

(use-package helm
  :custom
  (helm-semantic-fuzzy-match t)
  (helm-imenu-fuzzy-match    t)
  :config
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (setq helm-locate-command
        "glocate %s %s"
        helm-locate-create-db-command
        "gupdatedb --output='%s' --localpaths='%s'")
  (setq helm-locate-project-list
        (list "~/Dev"))
  )

(global-set-key (kbd "M-i") #'helm-imenu)
;; end_edit_and_movement

(use-package consult)

(general-create-definer zino/leader-key
  :prefix "SPC")

(defun zino/switch-other-buffer ()
  "Swithch to the most recent buffer . "
  (interactive)
  (switch-to-buffer (other-buffer)))

(zino/leader-key
  :keymaps 'normal
  "b" 'zino/switch-other-buffer)

(bind-key (kbd "C-c b") 'zino/switch-other-buffer)
(bind-key (kbd "C-c C-b") 'zino/switch-other-buffer)

(require 'hl-line)
(add-hook 'dired-mode-hook 'hl-line-mode)

(use-package move-text
  :config
  (move-text-default-bindings))

;; Set up before-save hooks to format buffer and add/delete imports .
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  "Befor-save hooks to format buffer."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook
  (go-mode . lsp-go-install-save-hooks))

;;(evil-define-key '(normal motion) go-mode-map "gd" 'evil-goto-definition)

;; begin_tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1)

(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-chunksize 2000)
;; end_tramp

;; begin_lsp
(use-package rust-mode)

(use-package lsp-treemacs
  :custom
  (lsp-treemacs-error-list-current-project-only t))

(defun zino/format-if-lsp-mode ()
  "Format on save if `lsp-mode' is present."
  (interactive)
  (if (and (bound-and-true-p lsp-mode)
           (not bound-and-true-p nginx-mode))
      (lsp-format-buffer)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-clangd-binary-path "/Library/Developer/CommandLineTools/usr/bin/clangd") ;; "/usr/local/opt/llvm/bin/clangd")
  (setq lsp-clients-lua-language-server-bin "/usr/local/bin/lua-language-server")
  :hook
  (
   (python-mode . lsp)
   (lua-mode . lsp)
   (c-mode . lsp)
   (c++-mode . lsp)
   (cmake-mode . lsp)
   (go-mode . lsp)
   (rust-mode . lsp)
   (python-mode . lsp)
   (jsonc-mode . lsp)
   (sh-mode . lsp)
   (html-mode . lsp)
   (css-mode . lsp)
   (javascript-mode . lsp)
   ;;; miscs
   (nginx-mode . lsp))
  :config
  ;;(define-key lsp-mode-map (kbd "C-c C-d") #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "C-c C-d") #'xref-find-definitions)
  (define-key lsp-mode-map (kbd "C-c d") #'xref-find-definitions-other-window)
  (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c C-e") #'flycheck-list-errors)
  (define-key lsp-mode-map (kbd "C-c C-r") #'lsp-find-references)
  (setq lsp-signature-auto-activate t
        lsp-signature-render-documentation t)
  ;; format-all does not work and it changes the window to the point of distracting ):
  (add-hook 'before-save-hook 'zino/format-if-lsp-mode)
  :custom
  (lsp-eldoc-enable-hover t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-signature-render-documentation t)
  (lsp-diagnostics-provider :flycheck)
  (lsp-eldoc-render-all t))

(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))

;;reset lsp session in lsp mode
;;(setq lsp--session nil)

(setq lsp-clients-clangd-args
      '("--header-insertion=iwyu"))

(setq lsp-intelephense-multi-root nil)

(use-package lsp-ui
  :after lsp-mode
  :commands
  lsp-ui-mode
  :custom-face
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
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
  (setq lsp-completion-provider :none)
  (define-key lsp-ui-imenu-mode-map (kbd "n") 'next-line)
  (define-key lsp-ui-imenu-mode-map (kbd "p") 'previous-line))

;; https://github.com/joaotavora/eglot/issues/98
(defun zino/project-try-cargo-toml (dir)
  "Try to locate a Rust project above DIR."
  (let ((found (locate-dominating-file dir "Cargo.toml")))
    (if (stringp found) `(transient . ,found) nil)))

;; Try rust projects before version-control (vc) projects
(add-hook 'project-find-functions 'zino/project-try-cargo-toml nil nil)

(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save
              nil)
            ;; Default depth of 0
            nil))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(perl-mode . ("/usr/local/Cellar/perl/5.34.0/lib/perl5/site_perl/5.34.0/Perl/LanguageServer.pm")))
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(cmake-mode . ("/home/cs144/.local/bin/cmake-language-server")))
  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))
  (add-to-list 'eglot-server-programs '(lua-mode . ("/usr/local/Cellar/lua-language-server/3.5.2/libexec/bin/lua-language-server")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("/usr/local/bin/rust-analyzer")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t))
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . xref-find-definitions)
        ("C-c r"   . eglot-rename)
        ("C-c C-r" . xref-find-references)
        ("C-c C-a" . eglot-code-actions))
  ;; :hook
  ;; (c-mode       . eglot-ensure)
  ;; (c++-mode     . eglot-ensure)
  ;; (cmake-mode   . eglot-ensure)
  ;; (sh-mode         . eglot-ensure)
  ;; (js-mode         . eglot-ensure)
  ;; (typescript-mode . eglot-ensure)
  )

;; (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/golang . org/x/lint/misc/emacs/"))
;; (require 'golint)
;; end_lsp

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package company-box
  :hook
  (company-mode . company-box-mode))

(use-package yasnippet
  :hook
  (after-init . yas-global-mode))

(use-package yasnippet-snippets)

(use-package flycheck
  :hook
  (lsp-mode . flycheck-mode))

(use-package flycheck-rust
  :hook
  (flycheck-mode . flycheck-rust-setup)
  :config
  (define-key flycheck-mode-map (kbd "C-c [") #'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "C-c ]") #'flycheck-next-error)
  )

;; (setq flycheck-indication-mode 'left-fringe)
;; (global-flycheck-mode)

;; TODO: use directory variables to configure per project
;; (add-hook 'c++-mode-hook (lambda ()
;;                            (setq flycheck-gcc-language-standard "c++17")))
;; (add-hook 'c++-mode-hook (lambda ()
;;                            (setq flycheck-clang-language-standard "c++17")))

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

(defvar isearch-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'isearch-repeat-forward)
    (define-key map "r" #'isearch-repeat-backward)
    map)
  "Repeat map for isearch.")

(repeatize 'isearch-repeat-map)
(repeatize 'flycheck-repeat-map)

;; Show matching parenthesis
(use-package mic-paren)
(paren-activate)

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

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Configure company-mode with BACKEND."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package button-lock)

;; begin_orgmode
(use-package org-noter
  :custom
  (org-noter-auto-save-last-location t)
  (org-noter-closest-tipping-point 1e-05)
  (org-noter-hide-other nil)
  (org-noter-notes-window-behavior '(scroll))
  )


(defun zino/org-journal-cycle-after-open-current-journal-file ()
  "Toggle visibility according to buffer's setting."
  (org-cycle t))

;; (add-hook 'org-mode-hook (lambda ()
;;                           (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

;;(add-function :after (symbol-function 'org-journal-open-current-journal-file) 'zino/org-journal-cycle-after-open-current-journal-file)
(advice-add 'org-journal-open-current-journal-file :after #'zino/org-journal-cycle-after-open-current-journal-file)

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c C-l") #'org-store-link)
(global-set-key (kbd "C-c i") #'org-insert-link)
(global-set-key (kbd "C-c t") #'org-cycle-list-bullet)

(setq org-agenda-include-diary t)
(setq org-agenda-todo-list-sublevels t)
(setq org-deadline-warning-days 5)

(setq zino/roam-dir "~/Notes/Roam")
(setq zino/anki-file "~/Notes/Roam/20220517104105-anki.org"
      zino/contacts-file "~/Notes/Roam/20220620203106-contacts.org"
      zino/meeting-file (concat zino/roam-dir "/20221115143855-meeting.org"))

(setq org-capture-templates
      `(
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

        ("c" "Contacts")
        ("cf" "Family"
         entry
         (file+headline zino/contacts-file "Family")
         "** %^{Name} \n:PROPERTIES:\n:CREATED: %(format-time-string \"<%Y-%m-%dT%H:%M>\" (current-time))\n:BIRTHDAY: %^{BIRTHDAY}\n:END:\n%?")
        ("cF" "Friends"
         entry
         (file+headline zino/contacts-file "Friends")
         "** %^{Name} \n:PROPERTIES:\n:CREATED: %(format-time-string \"<%Y-%m-%d:%H:%M>\" (current-time))\n:HowDoWeMeet: %^{How do we meet?}\n:END:\n%?")

        ("g" "Get Things Done")
        ("gq" "Questions"
         entry
         (file+regexp zino/GTD-file "\\* Questions \\[[0-9]*/[0-9]*\\]")
         "** TODO %^{What is the QUESTION} %^g\n:PROPERTIES:\n:CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n"
         :immediate-finish nil)
        ("gt" "Tasks"
         entry
         (file+regexp zino/GTD-file "\\* Tasks \\[[0-9]*/[0-9]*\\]")
         "** TODO %^{What is the TASK} %^g\n:PROPERTIES:\n:ID: %(shell-command-to-string \"uuidgen\"):CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n"
         :immediate-finish nil)
        ("gl" "Later"
         entry
         (file+regexp zino/GTD-file "\\* Later")
         "** TODO %^{What TO DO later} %^g\n:PROPERTIES:\n:CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n\n"
         :immediate-finish nil)

        ("w" "Weekly Plan")
        ("we" "Weekend"
         entry
         (file+function zino/GTD-file (lambda ()
                                        "Locate * Weekly Plan heading . Insert a timestamp of current week if there is none. create a TODO"
                                        (goto-char (point-min))
                                        (unless (re-search-forward "\\* Weekly Plan" (point-max) t)
                                          (goto-char (point-max))
                                          (newline)
                                          (insert "* Weekly Plan\n"))
                                        (unless (re-search-forward (concat "\\*\\* " (format-time-string "%Y-%m") "\\(-[0-9]*\\)?" (format-time-string "-W%V")) (point-max) t)
                                          (newline)
                                          (insert (concat "** " (format-time-string "%Y-%m-%d-W%V\n"))))))
         "*** TODO %^{What to do this weekend} %^g\n:PROPERTIES:\n:CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n\n")

        ("m" "Meeting")
        ("mr" "Random"
         entry
         (file+headline zino/meeting-file "Participated")
         "** %^{What is it about}  %^g\n:PROPERTIES:\n:CREATED: %<%Y-%m-%dT%H:%M>\n:END:\n"
         :immediate-finish nil
         :jump-to-captured t)))

(global-set-key (kbd "C-c c") #'org-capture)
;; end_orgmode

(setq request-log-level 'debug)

;; begin_anki_editor
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

(use-package anki-editor
  :hook
  (org-capture-after-finalize . zino/anki-editor-reset-cloze-num)
  :config
  ;; initialize cloze states
  (zino/anki-editor-reset-cloze-num)
  (setq anki-editor-create-decks t
        anki-editor-org-tags-as-anki-tags t)

  (general-define-key
   :prefix "C-c m"
   "i" #'anki-editor-insert-note
   "r" #'zino/anki-editor-reset-cloze-num
   "c" #'zino/anki-editor-cloze-region-auto-incr
   "p" #'zino/anki-editor-push-tree
   "b" #'zino/anki-editor-push-buffer))

;; check https://github.com/louietan/anki-editor/issues/76
;; end_anki_editor

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-quick-access ((t (:inherit company-tooltip-annotation :height 2.0))))
 '(cursor ((t (:background "#51afef"))))
 '(fixed-pitch ((t (:family "Fira Code" :height 250))))
 '(highlight-indent-guides-character-face ((t (:foreground "#3c3c42414e4d"))))
 '(highlight-indent-guides-top-character-face ((t (:foreground "#bbc2cf"))))
 '(hl-line ((t (:extend t :background "#42444a"))))
 '(lsp-ui-doc-background ((t (:inherit tooltip :background "#2e3138"))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
 '(org-block ((t (:inherit nil :extend t :background "#282c34"))))
 '(org-block-begin-line ((t (:inherit all-faces :foreground "#5B6268"))))
 '(org-checkbox-statistics-todo ((t (:inherit org-todo :family "Iosevka"))))
 '(org-code ((t (:inherit nil :foreground "#da8548"))))
 '(org-document-title ((t (:foreground "#c678dd" :weight bold :height 1.2 :family "Iosevka"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3 :width normal :family "Iosevka"))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.2 :width normal :family "Iosevka"))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :height 1.1 :width normal :family "Iosevka"))))
 '(org-level-4 ((t (:inherit outline-4 :extend nil :height 1.05 :width normal :family "Iosevka"))))
 '(org-level-5 ((t (:inherit outline-5 :extend nil :height 1.0 :width normal :family "Iosevka"))))
 '(org-level-6 ((t (:inherit outline-6 :extend nil :height 1.0 :width normal :family "Iosevka"))))
 '(org-link ((t (:inherit link :foreground "#51afef" :family "Iosevka"))))
 '(org-remark-highlighter ((t (:background "#023047"))))
 '(org-verbatim ((t (:foreground "#98be65"))))
 '(paren-face-match ((t (:foreground "#ff6c6b" :weight ultra-bold))))
 '(tooltip ((t (:background "#21242b" :foreground "#bbc2cf" :height 1.0))))
 '(variable-pitch ((t (:family "ETBembo" :height 250 :weight regular)))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comment-style 'indent)
 '(company-box-doc-delay 0.2)
 '(company-quickhelp-color-foreground nil)
 '(company-quickhelp-delay 0.01)
 '(company-quickhelp-mode t)
 '(company-quickhelp-use-propertized-text t)
 '(dired-create-destination-dirs 'always)
 '(dirvish-header-style 'normal)
 '(dirvish-preview-width 0.4)
 '(display-line-numbers-width nil)
 '(eldoc-echo-area-prefer-doc-buffer t)
 '(eldoc-idle-delay 0.2)
 '(helm-minibuffer-history-key "M-p")
 '(highlight-indent-guides-auto-character-face-perc 20)
 '(highlight-indent-guides-auto-top-character-face-perc 50)
 '(highlight-indent-guides-delay 0)
 '(highlight-indent-guides-responsive 'top)
 '(lsp-bridge-completion-popup-predicates
   '(lsp-bridge-not-only-blank-before-cursor lsp-bridge-not-match-hide-characters lsp-bridge-not-match-stop-commands lsp-bridge-not-in-string lsp-bridge-not-in-comment lsp-bridge-not-follow-complete lsp-bridge-multiple-cursors-disable lsp-bridge-not-complete-manually))
 '(lsp-clients-lua-lsp-server-install-dir "/usr/local/bin/lua-language-server")
 '(lsp-elm-only-update-diagnostics-on-save t)
 '(lsp-go-library-directories '("~/go/src/goroot" "/usr"))
 '(lsp-ui-doc-max-height 120)
 '(lsp-ui-doc-max-width 80)
 '(lsp-ui-doc-webkit-max-width-px 800)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-mac-link))
 '(org-remark-notes-file-name 'org-remark-notes-file-name-function)
 '(package-selected-packages
   '(forge anki-editor flycheck-rust flycheck lsp-treemacs fzf consult helm expand-region gn-mode company-graphviz-dot graphviz-dot-mode org-remark rust-mode lsp-ui eglot cape yaml-mode rime dired-rsync rg company org-roam-ui esup flymake-cursor mermaid-mode clipetty org lua-mode all-the-icons better-jumper org-notebook docker-tramp org-mac-link org-noter valign nov pdf-tools org-fragtog highlight-numbers rainbow-mode request beacon fixmee move-text go-mode popper cmake-mode dirvish fish-mode highlight-indent-guides indent-mode org-journal format-all filetags aggressive-indent agressive-indent elisp-format org-bars ws-butler emojify company-prescient prescien smartparents which-key visual-fill-column use-package undo-tree typescript-mode spacemacs-theme smartparens rainbow-delimiters pyvenv python-mode org-roam org-download org-bullets mic-paren magit lsp-ivy keycast ivy-yasnippet ivy-xref ivy-rich ivy-prescient helpful helm-xref helm-lsp gruvbox-theme git-gutter general flycheck-pos-tip evil-visualstar evil-surround evil-leader evil-collection doom-themes doom-modeline dap-mode counsel-projectile company-quickhelp company-posframe company-fuzzy company-box command-log-mode clang-format ccls base16-theme all-the-icons-dired))
 '(paren-display-message 'always)
 '(pdf-view-continuous t)
 '(pdf-view-display-size 'fit-page)
 '(pdf-view-resize-factor 1.1)
 '(popper-group-function 'popper-group-by-projectile)
 '(rime-cursor "˰")
 '(rime-posframe-style 'vertical)
 '(rime-show-candidate 'posframe)
 '(safe-local-variable-values
   '((lsp-lua-runtime-path .
                           ["/home/zino/gitlab/tengine-modules/?/?.lua" "?.lua" "?/init.lua" "?/?.lua"])))
 '(tab-always-indent nil)
 '(tab-bar-close-button-show nil)
 '(tab-bar-show nil)
 '(tab-width 4)
 '(tramp-default-proxies-alist nil)
 '(warning-suppress-types '((lsp-mode))))

(winner-mode)

;; the original keybinding is set-goal-column
(global-unset-key (kbd "C-x C-n"))

;; lua mode
(use-package lua-mode
  :bind
  (:map lua-mode-map
        ([remap beginning-of-defun] . lua-beginning-of-proc)
        ([remap end-of-defun] . lua-end-of-proc)))

(add-to-list 'load-path "~/.config/emacs/manually_installed/lua-mode")
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . perl-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(add-to-list 'auto-mode-alist '(".*Makefile" . makefile-mode))

(add-hook 'conf-mode-hook (lambda ()
                            "Set tab width to four spacess."
                            (setq tab-width 4)))
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
;; end_work_computer

;; begin_tramp
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
(setq tramp-verbose 1)
;; end_tramp

(global-set-key (kbd "<RET>") #'newline)

(global-set-key (kbd "C-M-k") #'sp-kill-sexp)
(global-set-key (kbd "C-s-k") #'sp-change-enclosing)

;; trying lsp-bridge
(use-package lsp-bridge
  :load-path "~/.config/emacs/manually_installed/lsp-bridge"
  ;; :hook
  ;;; statically typed
  ;; (c-mode . lsp-bridge-mode)
  ;; (c++-mode . lsp-bridge-mode)
  ;;; dynamically typed
  ;; (python-mode . lsp-bridge-mode)
  ;; (lua-mode . lsp-bridge-mode)
  ;; (go-mode . lsp-bridge-mode)
  ;; (sh-mode . lsp-bridge-mode)
  ;;; DSL
  ;; (cmake-mode . lsp-bridge-mode)
  ;; (css-mode . lsp-bridge-mode)
  ;; (html-mode . lsp-bridge-mode)
  :bind
  (:map lsp-bridge-mode-map
        ("C-c C-d" . lsp-bridge-find-define)
        ("C-c C-e" . lsp-bridge-list-diagnostics)
        ("C-o" . lsp-bridge-return-from-def)
        ("C-c C-o" . lsp-bridge-lookup-documentation)
        ("C-c C-v" . acm-doc-scroll-down)
        ("C-c C-M-v" . acm-doc-scroll-up))
  :custom
  (acm-doc-frame-max-lines 100))

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

(defun zino/no-op (&rest args))
(advice-add 'org-noter--set-notes-scroll :override 'zino/no-op)

;; this only undoes remapping instead of remapping to nil
;; (define-key global-map [remap ns-print-buffer] nil)

(global-unset-key (kbd "s-p"))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("C-c [" . flymake-goto-prev-error)
        ("C-c ]" . flymake-goto-next-error))
  :config
  (eval-after-load 'flymake '(require 'flymake-cursor)))
(use-package flymake-cursor)

;; git-modes
(use-package git-modes)

;; try ripgrep
(use-package rg
  :custom
  (rg-executable (executable-find "rg")))

(use-package dired-rsync
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (emacs-lisp . t)
   (shell . t)
   (lua . t)
   (dot . t)))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package nginx-mode)

;;; comint
(setq comint-prompt-read-only t)

;; set limit for prompt opening large files higher, 100 M
(setq large-file-warning-threshold 100000000)

;;; begin_helpers
(defun zino/save-buffer-and-exit()
  "Simple convenience function.
Save the buffer of the current window and kill it"
  (interactive)
  (save-buffer)
  (delete-window))
(global-set-key (kbd "C-x s-s") #'zino/save-buffer-and-exit)
;;; end_helpers

;; (add-hook 'eglot-managed-mode-hook (lambda ()
;;                                      (add-to-list 'company-backends
;;                                                   '(company-capf :with company-yasnippet))))

;; for corfu
;;(straight-use-package 'cape)

;; (defun my/eglot-capf ()
;;   (setq-local completion-at-point-functions
;;               (list (cape-super-capf
;;                      #'eglot-completion-at-point
;;                      (cape-company-to-capf #'company-yasnippet)))))

;;(use-package cape)
;;(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

;;; begin_comment
(global-set-key (kbd "C-;") #'comment-line)
;;; end_comment

(use-package format-all
  :hook
  ;; (rust-mode . format-all-mode)
  ;; (go-mode . format-all-mode)
  ;; (c++-mode . format-all-mode)
  (nginx-mode . format-all-mode))

(setq read-process-output-max (* 1024 1024)) ;; 1MB

;;; begin_better_jumper
(use-package better-jumper
  :config
  (better-jumper-mode 1)
  (setq better-jumper-context 'window)
  (global-set-key (kbd "C-o") #'better-jumper-jump-backward)

  ;; https://stackoverflow.com/questions/4512075/how-to-use-ctrl-i-for-an-emacs-shortcut-without-breaking-tabs
  (keyboard-translate ?\C-i ?\H-i)
  (global-set-key [?\H-i] #'better-jumper-jump-forward)

  ;; terminal emacs cannot differenciate C-i from tab
  (global-set-key (kbd "C-j") #'better-jumper-jump-forward))

(defun zino/set-jump-for-one-arg-fn (arg)
  "Set jump for functions with one ARG."
  (call-interactively #'better-jumper-set-jump))

(defun zino/set-jump-for-one-arg-one-opt-arg-fn (arg &optional OPT)
  "Set jump for functions with one ARG and one optional OPT."
  (call-interactively #'better-jumper-set-jump))

(defun zino/set-jump-for-one-option-arg (&optional ARG)
  "Set jump for functions with one optional ARG."
  (call-interactively #'better-jumper-set-jump))

(advice-add #'xref-find-definitions :before #'zino/set-jump-for-one-arg-fn)
(advice-add #'zino/switch-other-buffer :before #'better-jumper-set-jump)
(advice-add #'helm-imenu :before #'better-jumper-set-jump)
(advice-add #'widget-button-press :before #'zino/set-jump-for-one-arg-one-opt-arg-fn)
(advice-add #'org-open-at-point :before #'zino/set-jump-for-one-option-arg)
(advice-add #'beginning-of-buffer :before #'zino/set-jump-for-one-option-arg)
(advice-add #'end-of-buffer :before #'zino/set-jump-for-one-option-arg)

;; unable to set advice for `lsp-find-definition' for now
;; (advice-add #'lsp-find-definition :before #'zino/set-jump-for-lsp-find-def)
;; (advice-remove #'lsp-find-definition #'zino/set-jump-for-lsp-find-def)
;;; end_better_jumpter

(use-package org-remark
  :config
  ;; turn on `org-remark' highlights on startup
  (org-remark-global-tracking-mode 1)
  (keyboard-translate ?\C-m ?\H-m)
  (global-set-key (kbd "C-x H-m m") #'org-remark-mark)
  (with-eval-after-load 'org-remark
    (define-key org-remark-mode-map (kbd "C-x H-m o") #'org-remark-open)
    (define-key org-remark-mode-map (kbd "C-x H-m v") #'org-remark-view)
    (define-key org-remark-mode-map (kbd "C-x H-m ]") #'org-remark-view-next)
    (define-key org-remark-mode-map (kbd "C-x H-m [") #'org-remark-view-prev)
    (define-key org-remark-mode-map (kbd "C-x H-m r") #'org-remark-remove)
    (define-key org-remark-mode-map (kbd "C-x H-m d") #'org-remark-delete)
    (define-key org-remark-mode-map (kbd "C-x H-m y") #'org-remark-mark-yellow)
    (define-key org-remark-mode-map (kbd "C-x H-m l") #'org-remark-mark-red-line)))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4))

;; one tab for one workspace
(tab-bar-mode +1)

;; it is not common to mis-type this
(global-unset-key (kbd "C-<tab>"))
(global-set-key (kbd "M-<tab>") 'tab-next)
(global-set-key (kbd "M-S-<tab>") 'tab-previous)
(global-set-key (kbd "C-s-<tab>") 'tab-bar-select-tab-by-name)

(with-eval-after-load 'python-mode
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil))

;;; Google's gn meta build system
(use-package gn-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.gn\\'" . gn-mode)))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(defun zino/isearch-region-or-forward ()
  "isearch thing in region if region is active, otherwise perform normal isearch."
  (interactive)
  (if (use-region-p)
      (isearch-forward-thing-at-point)
    (isearch-forward)))

(global-set-key (kbd "C-s-s") #'zino/isearch-region-or-forward)
(put 'zino/isearch-region-or-forward 'repeat-map isearch-repeat-map)

(defun zino/inhibit-buffer-messages ()
  "Set `inhibit-message' buffer-locally."
  (setq-local inhibit-message t))
(add-hook 'pdf-view-mode-hook 'zino/inhibit-buffer-messages)

;; run as daemon
(server-start)

;; Put this at the end otherwise `pcache' will still register itself
(remove-hook 'kill-emacs-hook 'pcache-kill-emacs-hook)
(put 'narrow-to-region 'disabled nil)
