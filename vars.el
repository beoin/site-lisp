;; vars.el --- Variable settings.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Search
(setq lazy-highlight-initial-delay 5.00)

;; Minibuffer
(vertico-mode +1)
(marginalia-mode +1)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;; Org
(defvar org-agenda-files "~/Org/agenda-files.org")
(defvar org-directory "~/Org")
(defvar org-use-tag-inheritance nil)
(defvar org-hide-emphasis-markers t)
(defvar org-startup-folded t)
(defvar org-return-follows-link nil)
(global-auto-revert-mode t)
(defvar org-imenu-depth 3)

;; Sessions
(savehist-mode +1)

;; UI
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")
(setq tab-bar-show t)
(blink-cursor-mode 0)
(line-number-mode +1)
(column-number-mode +1)
(global-display-line-numbers-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

;; Files
(defvar view-read-only t)
(defvar view-scroll-auto-exit t)

;; Movement
(setq set-mark-command-repeat-pop t)

;; Custom File
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Company
(defvar company-idle-delay nil)

;; Programming
(electric-pair-mode)
(defvar xref-show-definitions-function #'xref-show-definitions-completing-read)
(defvar flymake-shellcheck-path "usr/bin/shellcheck")
(global-flycheck-mode +1)

;; Version Control
(setq vc-follow-symlinks nil)

;; Text Editing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(delete-selection-mode t)
(which-function-mode t)

;; Shell
(defvar eshell-aliases-file "~/.emacs.d/eshell/alias")

;; Location of "~" backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Grep
(setq grep-command  "grep -r --color=auto -nH --null -e" )

;; Go
(defvar treesit-language-source-alist '((gomod "https://github.com/camdencheek/tree-sitter-go-mod")))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(provide 'vars)
;;; vars.el ends here
