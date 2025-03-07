;;; c-conf.el --- C configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Imports
(require 'cc-mode)

;; Vars
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))

;; Hooks
(add-hook 'c-ts-mode-hook
          (defun c-hook ()
            (keymap-local-set "C-a" #'beginning-of-line-text)
            (auto-revert-mode)))

(provide 'c-conf)
;;; c-conf.el ends here.
