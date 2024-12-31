;;; go-conf.el --- Go configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Imports
(require 'go-mode)

;; Vars
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(provide 'go-conf)
;;; go-conf.el ends here
