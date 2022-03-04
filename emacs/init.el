;;; init.el --- Startup file for emacs -*- lexical-binding: t -*-
;;;
;;; Load configuration file from another org file
;;;
;;; Code:

(setq user-emacs-directory "~/.config/emacs/")
(org-babel-load-file (concat user-emacs-directory "config.org"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
 '(org-checkbox-statistics-todo ((t (:inherit org-todo :family "Fira Mono"))))
 '(org-document-title ((t (:foreground "#c678dd" :weight bold :height 1.2 :family "Iosevka"))))
 '(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3 :width extra-expanded :family "Iosevka"))))
 '(org-level-2 ((t (:inherit outline-2 :extend nil :height 1.2 :width extra-expanded :family "Iosevka"))))
 '(org-level-3 ((t (:inherit outline-3 :extend nil :height 1.1 :width extra-expanded :family "Iosevka"))))
 '(org-level-4 ((t (:inherit outline-4 :extend nil :height 1.05 :width extra-expanded :family "Iosevka")))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" default))
 '(display-line-numbers-type 'relative)
 '(elisp-format-define-keyword-list
   '("defun" "defun*" "defsubst" "defmacro" "defadvice" "define-skeleton" "define-minor-mode" "define-global-minor-mode" "define-globalized-minor-mode" "define-derived-mode" "define-generic-mode" "define-compiler-macro" "define-modify-macro" "defsetf" "define-setf-expander" "define-method-combination" "defgeneric" "defmethod" "defalias" "defvar" "defconst" "defconstant" "defcustom" "defparameter" "define-symbol-macro" "defgroup" "deftheme" "deftype" "defstruct" "defclass" "define-condition" "define-widget" "defface" "defpackage" ":init"))
 '(elisp-format-newline-keyword-addons-list
   '("interactive" "setq" "set" "buffer-substring" "buffer-substring-no-properties" "progn"))
 '(elisp-format-split-subexp-keyword-addons-list
   '("and" "or" "buffer-substring" "buffer-substring-no-properties" "font-lock-add-keywords" ":init" "test"))
 '(elisp-format-split-subexp-keyword-except-list
   '("provide" "require" "loop" "throw" "featurep" "use-package"))
 '(elisp-format-split-subexp-keyword-keep-alist
   '((1 "and" "or" "let" "let*" "while" "when" "catch" "unless" "if" "dolist" "dotimes" "lambda" "cond" "condition-case" "with-current-buffer" "with-temp-message" "with-selected-window" "with-output-to-temp-buffer" "with-selected-frame" "define-minor-mode" "set" "setq")
     (2 "defun" "defun*" "defsubst" "defmacro" "defadvice" "define-skeleton" "define-globalized-minor-mode" "define-derived-mode" "define-generic-mode" "define-compiler-macro" "define-modify-macro" "defsetf" "define-setf-expander" "define-method-combination" "defgeneric" "defmethod" "defalias" "defvar" "defconst" "defconstant" "defcustom" "defparameter" "define-symbol-macro" "defgroup" "deftheme" "deftype" "defstruct" "defclass" "define-condition" "define-widget" "defface" "defpackage")
     (5 "loop")))
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "~")
 '(global-company-mode t)
 '(helm-minibuffer-history-key "M-p")
 '(highlight-indent-guides-responsive 'top)
 '(package-selected-packages
   '(dirvish fish-mode highlight-indent-guides indent-mode org-journal format-all filetags aggressive-indent agressive-indent elisp-format org-bars ws-butler emojify company-prescient prescien smartparents which-key visual-fill-column use-package undo-tree typescript-mode spacemacs-theme smartparens rainbow-delimiters pyvenv python-mode org-roam org-download org-bullets mic-paren magit lsp-ui lsp-ivy keycast ivy-yasnippet ivy-xref ivy-rich ivy-prescient helpful helm-xref helm-lsp gruvbox-theme git-gutter general flycheck-pos-tip evil-visualstar evil-surround evil-leader evil-collection eglot doom-themes doom-modeline dap-mode counsel-projectile company-quickhelp company-posframe company-fuzzy company-box command-log-mode clang-format ccls base16-theme all-the-icons-dired)))
(put 'narrow-to-region 'disabled nil)
