;;; vars.el --- Variables without a dedicated config file.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defvar config-files (concat user-emacs-directory "lisp" )
  "Configuration files path.")

;; commands, docs, vars and functions apropos to supplied term
(require 'apropos)
(setq apropos-sort-by-scores 'verbose)
(setq apropos-compact-layout nil)
(setq apropos-documentation-sort-by-scores 't)
(setq apropos-do-all nil)

;; incremental search
(require 'isearch)
(setq lazy-highlight-initial-delay 5.00)
(setq isearch-lazy-count 't)
(setq lazy-count-prefix-format nil)
(setq lazy-count-suffix-format "   (%s/%s)")
(setq search-ring-max 20)

;; text expansion
(require 'abbrev)
(abbrev-mode -1)

;; framework for mode-specific buffer indexes
(require 'imenu)
(setq imenu-auto-rescan 't)
(setq imenu-use-popup-menu nil)
(setq imenu-flatten nil)
(setq imenu-space-replacement "_")

;; utility functions for multilingual environment (mule)
(require 'mule-util)
(setq truncate-string-ellipsis "…")

;; pulse modified regions
(require 'goggles)
(setq-default goggles-pulse 't)

;; interface for display-line-numbers
(require 'display-line-numbers)
(setq display-line-numbers-grow-only 't)

;; basic editing commands
(require 'simple)
(size-indication-mode)
(column-number-mode)
(setq next-line-add-newlines nil)
(setq set-mark-command-repeat-pop 't)
(setq kill-do-not-save-duplicates 't)
(setq blink-matching-paren 'jump)
(setq shell-command-prompt-show-cwd 't)
(put 'set-goal-column 'disabled nil)

;; fns.c Random utility lisp functions
(setq use-dialog-box nil)
(setq use-short-answers 't)

;; delete selection if you insert
(require 'delsel)
(delete-selection-mode)

;; print current function in mode line
(require 'which-func)
(which-function-mode)
(setq which-func-update-delay 1.0)
(setq which-func-display 'mode)

;; help commands
(require 'help)
(setq help-window-keep-selected 't)
(setq help-window-select nil)
(setq describe-bindings-show-prefix-commands 't)

;; help-mode used by *Help* buffers
(require 'help-mode)
(setq help-clean-buttons 't)

;; complex help functions
(require 'help-fns)
(setq help-enable-variable-value-editing nil)
(put 'help-fns-edit-variable 'disabled nil)

;; window system-independent mouse support
(require 'mouse)
(setq mouse-yank-at-point 't)

;; dispnew.c updating of data structures for redisplay.
(setq visible-bell nil)

;; an outline can be abstracted to show headers at any given level
(require 'outline)
(setq outline-minor-mode-cycle 't)

;; frame.c generic frame functions
(setq-default window-resize-pixelwise 't)
(setq-default frame-resize-pixelwise 't)

;; xdisp.c display generation from window structure and buffer text.
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq x-stretch-cursor nil)
(setq max-mini-window-height 10.00)
(setq message-log-max 250)
(setq resize-mini-windows 'grow-only)

;; window.c window creation, deletion and examination
(setq scroll-preserve-screen-position 1)
(put 'scroll-left 'disabled nil)

;; multi-frame management independent of window systems
(require 'frame)
(blink-cursor-mode)

;; generic major mode for programming
(require 'prog-mode)
(setq prettify-symbols-unprettify-at-point 't)

;; (un)comment regions of buffers
(require 'newcomment)
(setq comment-padding 1)
(setq comment-empty-lines nil)

;; automatic parenthesis pairing
(require 'elec-pair)
(electric-pair-mode)
(setq electric-pair-pairs '((?\< . ?\>)
                            (34 . 34)
                            (8216 . 8217)
                            (8220 . 8221)
                            (?\{ . ?\})))

;; casefiddle.c case conversion functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; an overloading mechanism for lisp functions
(require 'advice)
(setq ad-redefinition-action 'error)

;; paragraph and sentence parsing
;;(require 'paragraph) not provided
(setq sentence-end-double-space nil)

;; run grep and display the results
(require 'grep)
(setq grep-command  "grep -r --color=auto -nH --null -e" )
(setq grep-use-headings 't)

;; tree-sitter utilities
(require 'treesit)
(setq treesit-language-source-alist '())
(setq treesit-font-lock-level 4)

;; run compiler, parse error messages
(require 'compile)
(setq compilation-always-kill 't)

;; snippet extensions
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/lisp/snippets"))

;; indentation commands
;;(require 'indent) not provided
(setq tab-always-indent 't)
(setq-default indent-tabs-mode nil)

;; frame-local tabs with named persistent window configurations
(require 'tab-bar)
(setq tab-bar-show 't)

;; interface to spell checkers
(require 'ispell)
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.aspell.en.pws")
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-silently-savep 't)
(add-to-list 'auto-mode-alist '("\\.pws\\'" . text-mode))

;; on-the-fly spell checker
(require 'flyspell)
(setq flyspell-abbrev-p 't)
(setq flyspell-use-global-abbrev-table-p 't)
(setq flyspell-issue-message-flag nil)
(setq flyspell-issue-welcome-flag nil)

;; string and regular-expression replace commands
(require 'replace)
(setq list-matching-lines-jump-to-current-line 't)
(setq list-matching-lines-default-context-lines 0)

;; some nonstandard editing and utility commands
(require 'misc)
(setq duplicate-line-final-position 1)
(setq duplicate-region-final-position 1)

;; cross-referencing commands
(require 'xref)
(setq xref-after-jump-hook '(recenter xref-pulse-momentarily view-mode-enter))
(setq xref-show-definitions-function 'xref-show-definitions-completing-read)

(provide 'vars)
;;; vars.el ends here
