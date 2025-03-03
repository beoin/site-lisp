;;; vars.el --- Variable settings.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Search
(setq lazy-highlight-initial-delay 5.00)
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")

;; Imenu
(defvar imenu-auto-rescan t)
(defvar org-imenu-depth 2)
(defvar imenu-use-popup-menu nil)
(defvar imenu-flatten 'prefix)

;; Sessions
(savehist-mode +1)

;; UI
(defvar truncate-string-ellipsis "…")
(column-number-mode +1)
(setq-default goggles-pulse t)
(defvar display-line-numbers-grow-only t)
(setq use-dialog-box nil)

;; Editor
(delete-selection-mode t)
(setq confirm-kill-emacs #'yes-or-no-p)
(defvar next-line-add-newlines nil)

;; Which Function
(require 'which-func)
(which-function-mode)
(setq which-func-update-delay 1.0)
(defvar which-func-display 'mode)

;; Documentation
(require 'help-fns)
(setq help-clean-buttons t)
(setq help-window-keep-selected t)
(setq help-window-select t)
(setq help-enable-variable-value-editing nil)
(setq describe-bindings-show-prefix-commands t)
(put 'help-fns-edit-variable 'disabled nil)

;; UX
(setq ring-bell-function 'ignore)
(setq mouse-yank-at-point t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq visible-bell nil)
(defvar outline-minor-mode-cycle t)
(setq-default indicate-empty-lines t)

;; Frames
(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)

;; Scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)
(put 'scroll-left 'disabled nil)

;; Cursor
(blink-cursor-mode 0)
(setq cursor-type 'box)
(setq x-stretch-cursor nil)

;; Modeline
(doom-modeline-mode 1)

;; Files
(setq large-file-warning-threshold 100000000)
(defvar recentf-max-saved-items 50)
(defvar recentf-max-menu-items 20)
(setq delete-by-moving-to-trash t)
(setq find-file-suppress-same-file-warnings t)
(defvar archive-visit-single-files t)

;; Backups
(setq backup-directory-alist '(("." . ".~")))
(setq version-control 'never)
(setq delete-old-versions t)

;; Custom File
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Marks
(setq set-mark-command-repeat-pop t)

;; Programming
(defvar xref-show-definitions-function #'xref-show-definitions-completing-read)
(defvar flymake-shellcheck-path "usr/bin/shellcheck")
(setq prettify-symbols-unprettify-at-point t)
(defvar comment-empty-lines nil)
(defvar comment-padding 1)

;; Electric pair mode
(electric-pair-mode +1)
(setq electric-pair-pairs '((?\< . ?\>)
                            (34 . 34)
                            (8216 . 8217)
                            (8220 . 8221)
                            (?\{ . ?\})))

;; Version Control
(setq vc-follow-symlinks nil)

;; Text Editing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(setq ad-redefinition-action 'error)
(setq-default fill-column 80)
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq sentence-end-double-space nil)
(setq require-final-newline t)

;; Shell
(defvar eshell-aliases-file "~/.emacs.d/eshell/alias")
(defvar flycheck-sh-shellcheck-executable "/usr/bin/shellcheck")
(setq major-mode-remap-alist '((sh-mode . bash-ts-mode)))

;; Grep
(setq grep-command  "grep -r --color=auto -nH --null -e" )
(defvar grep-use-headings t)

;; Find
(setq find-name-arg "-iname")

;; Tree Sitter
(defvar treesit-language-source-alist '())
(defvar treesit-font-lock-level 4)

;; Compilation
(defvar compilation-always-kill t)

;; Snippets
(defvar yas-snippet-dirs '("~/.emacs.d/lisp/snippets"))

;; Tabs
(setq tab-bar-show t)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; Spell Checking
(defvar ispell-program-name "aspell")
(defvar ispell-personal-dictionary "~/.aspell.en.pws")
(defvar ispell-extra-args '("--sug-mode=ultra"))
(defvar flyspell-abbrev-p t)
(defvar flyspell-use-global-abbrev-table-p t)
(defvar flyspell-issue-message-flag nil)
(defvar flyspell-issue-welcome-flag nil)
(add-to-list 'auto-mode-alist '("\\.pws\\'" . text-mode))

;; Find file at point
(defvar ffap-machine-p-known 'reject)

(provide 'vars)
;;; vars.el ends here
