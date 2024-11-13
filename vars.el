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
(setq use-short-answers t)
(setq enable-recursive-minibuffers t)

;; Org
(defvar org-agenda-files "~/Org/agenda-files.org")
(defvar org-directory "~/Org")
(defvar org-default-notes-file (concat org-directory "/capture_notes.org"))
(defvar org-use-tag-inheritance nil)
(defvar org-hide-emphasis-markers t)
(defvar org-startup-folded t)
(defvar org-return-follows-link nil)
(global-auto-revert-mode t)
(defvar org-rainbow-tags-hash-start-index 10)

;; Imenu
(defvar imenu-auto-rescan t)
(defvar org-imenu-depth 2)

;; Sessions
(savehist-mode +1)

;; UI
(defvar truncate-string-ellipsis "…")
(defvar which-func-update-delay 1.0)
(setq tab-bar-show t)
(line-number-mode +1)
(column-number-mode +1)
(global-display-line-numbers-mode -1)
(setq ring-bell-function 'ignore)
(setq require-final-newline t)
(global-hl-line-mode +1)
(setq-default goggles-pulse t)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(defvar modus-themes-common-palette-overrides
      '((fg-heading-1 "#A3C7D6")))

;; Cursor
(blink-cursor-mode 0)
(setq cursor-type 'box)

;; Modeline
(doom-modeline-mode 1)

;; Buffers
(defvar view-read-only t)
(defvar view-scroll-auto-exit t)

;; Files
(global-auto-revert-mode t)
(setq large-file-warning-threshold 100000000)
(save-place-mode 1)
(defvar recentf-max-saved-items 50)
(defvar recentf-max-menu-items 20)
(recentf-mode 1)
(defvar find-file-visit-truename t)

;; Custom File
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Marks
(setq set-mark-command-repeat-pop t)

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
(setq ad-redefinition-action 'error)

;; Shell
(defvar eshell-aliases-file "~/.emacs.d/eshell/alias")
(defvar flycheck-sh-shellcheck-executable "/usr/bin/shellcheck")

;; Location of "~" backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Grep
(setq grep-command  "grep -r --color=auto -nH --null -e" )

;; Tree Sitter
(defvar treesit-language-source-alist
  '((zig "https://github.com/maxxnino/tree-sitter-zig")))
(defvar treesit-font-lock-level 4)

;; Zig
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))

;; Snippets
(defvar yas-snippet-dirs '("~/.emacs.d/lisp/snippets"))
(yas-global-mode)

;; Dired
(lisp-extra-font-lock-global-mode 1)
(put 'dired-find-alternate-file 'disabled nil)
(defvar dired-recursive-deletes 'always)
(defvar dired-recursive-copies 'always)

;; Tabs
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; Paren Matching
(setq blink-matching-paren 'jump)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-style 'parenthesis)

;; Spell Checking
(defvar ispell-program-name "aspell")
(defvar ispell-personal-dictionary "~/.aspell.en.pws")
(defvar ispell-extra-args '("--sug-mode=ultra"))

;; Find file at point
(defvar ffap-machine-p-known 'reject)

(provide 'vars)
;;; vars.el ends here
