;;; go-conf.el --- Go configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Imports
(require 'go-mode)

;; Vars
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

;; Hooks
(add-hook 'go-ts-mode
          (defun go-hook ()
            (keymap-local-set "C-c f" #'gofmt)
            (keymap-local-set "<f7>" #'goto-line)))

(provide 'go-conf)
;;; go-conf.el ends here
