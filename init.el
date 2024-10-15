;;; init.el --- Entry point for config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path  "~/.emacs.d/lisp")
(require 'pkg-mgt)
(require 'vars)
(require 'functions)
(require 'editing-functions)
(require 'hooks)
(require 'keybindings)

(load custom-file)

(find-file-noselect "~/.bashrc")
(find-file-noselect "~/.config/sway/config")
(find-file "~/Org/todo.org")

(require 'doom-modeline)
(doom-modeline-mode 1)

(require 'lisp-extra-font-lock)
(lisp-extra-font-lock-global-mode 1)

(provide 'init)
;;; init.el ends here
