;;; buffers-conf.el --- buffer related configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Vars
(defvar view-read-only t)
(defvar view-scroll-auto-exit t)
(defvar switch-to-buffer-in-dedicated-window 'pop)
(defvar switch-to-buffer-obey-display-actions t)
(setq display-buffer-alist nil)

;; Ibuffer
(defvar ibuffer-movement-cycle nil)
(defvar ibuffer-old-time 24)
(defvar ibuffer-default-sorting-mode 'alphabetic)
(defvar ibuffer-expert nil)

;; Minibuffer
(vertico-mode +1)
(marginalia-mode +1)
(which-key-mode)
(which-key-setup-minibuffer)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))
(setq use-short-answers t)
(setq enable-recursive-minibuffers t)
(setq kill-do-not-save-duplicates t)
(defvar vertico-count 15)
(setq max-mini-window-height 10.00)
(defvar embark-confirm-act-all t)
(defvar minibuffer-visible-completions t)

;;Keybindings
(keymap-global-set "C-x B" 'ibuffer-list-buffers)
(keymap-global-set "C-." 'embark-act)

(provide 'buffers-conf)
;;; buffers-conf.el ends here
