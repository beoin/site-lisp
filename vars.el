;;; vars.el --- Variables without a dedicated config file.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar config-files (concat user-emacs-directory "lisp" )
  "Configuration files path.")

;; incremental search minor mode
(require 'isearch)
(setq lazy-highlight-initial-delay 5.00)
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")
(setq search-ring-max 20)

;; Imenu
(defvar imenu-auto-rescan t)
(defvar org-imenu-depth 2)
(defvar imenu-use-popup-menu nil)
(defvar imenu-flatten nil)

;; Sessions
(savehist-mode +1)

;; UI
(defvar truncate-string-ellipsis "…")
(setq-default goggles-pulse t)
(defvar display-line-numbers-grow-only t)
(setq use-dialog-box nil)

;; mode line
(size-indication-mode)
(column-number-mode +1)

;; Editor
(delete-selection-mode t)
(setq confirm-kill-emacs #'yes-or-no-p)
(defvar next-line-add-newlines nil)

;; Which Function
(require 'which-func)
(which-function-mode)
(setq which-func-update-delay 1.0)
(defvar which-func-display 'mode)

;; The Help System
(require 'help)
(setq help-window-keep-selected t)
(setq help-window-select nil)
(setq describe-bindings-show-prefix-commands t)

;; `help-mode' used by *Help* buffers
(require 'help-mode)
(setq help-clean-buttons t)

;; Complex help functions
(require 'help-fns)
(setq help-enable-variable-value-editing nil)
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

;; Marks
(setq set-mark-command-repeat-pop t)

;; Programming
(defvar xref-show-definitions-function 'xref-show-definitions-completing-read)
(setq prettify-symbols-unprettify-at-point t)
(setq comment-empty-lines nil)
(setq comment-padding 1)

;; Electric pair mode
(require 'elec-pair)
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

;; Grep
(setq grep-command  "grep -r --color=auto -nH --null -e" )
(defvar grep-use-headings t)

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
(setq ispell-personal-dictionary "~/.aspell.en.pws")
(defvar ispell-extra-args '("--sug-mode=ultra"))
(defvar flyspell-abbrev-p t)
(defvar flyspell-use-global-abbrev-table-p t)
(defvar flyspell-issue-message-flag nil)
(defvar flyspell-issue-welcome-flag nil)
(add-to-list 'auto-mode-alist '("\\.pws\\'" . text-mode))

;; Find file at point
(defvar ffap-machine-p-known 'reject)

;; Cross-referencing commands
(require 'xref)
(setq xref-after-jump-hook '(recenter xref-pulse-momentarily view-mode-enter))

(provide 'vars)
;;; vars.el ends here
