;;; roc-conf.el --- Roc configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Imports
(require 'roc-ts-mode)

;; Vars
(add-to-list 'auto-mode-alist '("\\.roc\\'" . roc-ts-mode))

;; Hooks
(add-hook 'roc-ts-mode-hook
          (defun roc-hook ()
           (keymap-local-set "C-c f" #'roc-ts-format)))

          
(provide 'roc-conf)
;;; roc-conf.el ends here
