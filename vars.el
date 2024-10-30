;; vars.el --- Variable settings.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Search
(setq lazy-highlight-initial-delay 5.00)
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

;; Minibuffer
(vertico-mode +1)
(marginalia-mode +1)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))
(fset 'yes-or-no-p 'y-or-n-p)

;; Org
(defvar org-agenda-files "~/Org/agenda-files.org")
(defvar org-directory "~/Org")
(defvar org-default-notes-file (concat org-directory "/capture_notes.org"))
(defvar org-use-tag-inheritance nil)
(defvar org-hide-emphasis-markers t)
(defvar org-startup-folded t)
(defvar org-return-follows-link nil)
(global-auto-revert-mode t)
(defvar org-imenu-depth 2)

;; Sessions
(savehist-mode +1)

;; UI
(setq tab-bar-show t)
(line-number-mode +1)
(column-number-mode +1)
(global-display-line-numbers-mode -1)
(setq ring-bell-function 'ignore)
(setq require-final-newline t)
(global-hl-line-mode +1)

;; Cursor
(blink-cursor-mode 0)
(setq cursor-type 'box)

;; Modeline
(doom-modeline-mode 1)

;; Files
(defvar view-read-only t)
(defvar view-scroll-auto-exit t)
(global-auto-revert-mode t)
(setq large-file-warning-threshold 100000000)
(save-place-mode 1)
(recentf-mode 1)
(defvar recentf-max-saved-items 50)
(defvar recentf-max-menu-items 20)

;; Movement
(setq set-mark-command-repeat-pop t)

;; Custom File
(setq custom-file (concat user-emacs-directory "custom.el"))

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
(defvar flycheck-sh-shellcheck-executable "/usr/bin/shellcheck")

;; Location of "~" backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Grep
(setq grep-command  "grep -r --color=auto -nH --null -e" )

;; Tree Sitter
(defvar treesit-language-source-alist
  '((heex "https://github.com/phoenixframework/tree-sitter-heex")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")))

;; Elixir
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))

;; Snippets
(setq yas-snippet-dirs '("~/.emacs.d/lisp/snippets"))
(yas-global-mode)

;; Dired
(lisp-extra-font-lock-global-mode 1)

;; Tabs
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; Paren Matching
(setq blink-matching-paren 'jump)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-style 'parenthesis)



(provide 'vars)
;;; vars.el ends here
