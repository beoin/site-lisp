;;; Init.el --- Entry point for config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path  "~/.emacs.d/lisp")
(require 'pkg-mgt)
(require 'vars)
(require 'functions)
(require 'editing-functions)
(require 'hooks)
(require 'keybindings)
(require 'org-mode-conf)

(load custom-file)

(find-file-noselect "~/.bashrc")
(find-file-noselect "~/.config/sway/config")
(find-file "~/Org/todo.org")

(provide 'init)
;;; init.el ends here
