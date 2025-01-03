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
(require 'org-conf)
(require 'dired-conf)
(require 'buffers-conf)
(require 'lisp-conf)
(require 'go-conf)

(load custom-file)

(find-file-noselect "~/.bashrc")
(find-file-noselect "~/.config/sway/config")
(find-file "~/Org/todo.org")

(pdf-tools-install)

(provide 'init)
;;; init.el ends here
