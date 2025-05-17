;;; c-conf.el --- C configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Imports
(require 'cc-mode)

;; tree-sitter support for C and C++
(require 'c-ts-mode)
(setq c-ts-mode-emacs-sources-support 't)
(setq c-ts-mode-indent-offset 2)
(setq c-ts-mode-indent-style 'gnu)

;; Vars
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))

;; Hooks
(add-hook 'c-ts-mode-hook
          (defun c-hook ()
            (auto-revert-mode)
            (electric-pair-mode)
            (keymap-local-set "C-j" #'open-line-below-point)
            (keymap-local-set "C-c b" #'c-insert-comment)))

;; Functions
(defun c-insert-comment ()
  "Insert a C comment block at point."
  (interactive)
  (insert "/*   */")
  (backward-char 4))

(provide 'c-conf)
;;; c-conf.el ends here.
