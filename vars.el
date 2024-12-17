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
(setq kill-do-not-save-duplicates t)
(defvar vertico-count 15)
(which-key-mode)
(which-key-setup-minibuffer)
(setq max-mini-window-height 10.00)

;; Imenu
(defvar imenu-auto-rescan t)
(defvar org-imenu-depth 2)

;; Sessions
(savehist-mode +1)

;; UI
(defvar truncate-string-ellipsis "…")
(defvar which-func-update-delay 1.0)
(line-number-mode +1)
(column-number-mode +1)
(global-display-line-numbers-mode -1)
(global-hl-line-mode +1)
(setq-default goggles-pulse t)
(defvar display-line-numbers-grow-only t)
(defvar display-line-numbers-width 2)

;; Editor
(delete-selection-mode t)
(which-function-mode t)
(setq confirm-kill-emacs #'yes-or-no-p)

;; UX
(setq ring-bell-function 'ignore)
(setq mouse-yank-at-point t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq visible-bell nil)

;; Frames
(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)

;; Scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

;; Cursor
(blink-cursor-mode 0)
(setq cursor-type 'box)
(setq x-stretch-cursor nil)

;; Modeline
(doom-modeline-mode 1)

;; Buffers
(defvar view-read-only t)
(defvar view-scroll-auto-exit t)
(defvar ibuffer-old-time 24)
(defvar switch-to-buffer-in-dedicated-window 'pop)
(defvar switch-to-buffer-obey-display-actions t)
(defvar ibuffer-movement-cycle nil)

;; Files
(global-auto-revert-mode t)
(setq large-file-warning-threshold 100000000)
(defvar recentf-max-saved-items 50)
(defvar recentf-max-menu-items 20)
(recentf-mode 1)
(defvar find-file-visit-truename t)
(setq delete-by-moving-to-trash t)
(setq comment-empty-lines t)
(setq find-file-suppress-same-file-warnings t)

;; Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)
(setq delete-old-versions t)

;; Custom File
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Marks
(setq set-mark-command-repeat-pop t)

;; Programming
(electric-pair-mode)
(defvar xref-show-definitions-function #'xref-show-definitions-completing-read)
(defvar flymake-shellcheck-path "usr/bin/shellcheck")
(global-flycheck-mode +1)
(setq prettify-symbols-unprettify-at-point t)

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

;; Grep
(setq grep-command  "grep -r --color=auto -nH --null -e" )

;; Find
(setq find-name-arg "-iname")

;; Tree Sitter
(defvar treesit-language-source-alist
  '((zig "https://github.com/maxxnino/tree-sitter-zig")))
(defvar treesit-font-lock-level 4)

;; Zig
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))

;; Rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(require 'rust-mode)

;; Compilation
(defvar compilation-always-kill t)

;; Snippets
(defvar yas-snippet-dirs '("~/.emacs.d/lisp/snippets"))
(yas-global-mode)

;; Dired
(lisp-extra-font-lock-global-mode 1)
(put 'dired-find-alternate-file 'disabled nil)
(defvar dired-recursive-deletes 'always)
(defvar dired-recursive-copies 'always)
(defvar dired-create-destination-dirs 'ask)
(defvar dired-ls-F-marks-symlinks t)
(defvar dired-deletion-confirmer 'y-or-n-p)
(defvar dired-guess-shell-alist-user
  '(("\.pdf$" "xpdf")
    ("\.html$" "firefox")
    ("\.mp3$\\|.mp4$\\|.mkv$\\|" "mpv")
    ("\.epub$\\|.mobi$\\|.azw3$|" "ebook-viewer")))

;; Tabs
(setq tab-bar-show t)
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
(defvar flyspell-abbrev-p t)
(defvar flyspell-use-global-abbrev-table-p t)
(defvar flyspell-issue-message-flag nil)
(defvar flyspell-issue-welcome-flag nil)

;; Find file at point
(defvar ffap-machine-p-known 'reject)

(provide 'vars)
;;; vars.el ends here
