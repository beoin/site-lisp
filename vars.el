;; vars.el --- Variable settings.  -*- lexical-binding: t -*-

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.gohtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . web-mode))
;;(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-pairing t)

;; Search
(setq lazy-highlight-initial-delay 5.00)

;; Minibuffer
(vertico-mode +1)
(marginalia-mode +1)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;; Org
(setq org-agenda-files "~/Org/agenda-files.org")
(setq org-directory "~/Org")
(setq org-use-tag-inheritance nil)
(setq org-hide-emphasis-markers t)
(setq org-startup-folded t)
(setq org-return-follows-link nil)
(global-auto-revert-mode t)
(setq org-imenu-depth 3)

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
(setq modus-themes-common-palette-overrides
      '((fg-heading-1 "#A3C7D6")))

;; Files
(setq view-read-only t)

;; Movement
(setq set-mark-command-repeat-pop t)

;; Custom File
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Company
(setq company-idle-delay nil)

;; Programming
(setq zig-zig-bin "~/Zig/zig-master/zig")
(electric-pair-mode)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(setq flymake-shellcheck-path "~/.local/bin/shellcheck")
(setq gofmt-command "goimports")

;; Version Control
(setq vc-follow-symlinks nil)

;; Text Editing
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(delete-selection-mode t)
(which-function-mode t)

;; Shell
(setq eshell-aliases-file "~/.emacs.d/eshell/alias")

;; Snippets
(yas-global-mode 1)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . js-mode))

;; Common Lisp
(setq inferior-lisp-program "/usr/bin/sbcl")

(provide 'vars)
;;; vars.el ends here
