;;; shell-conf.el --- Shell configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(setq major-mode-remap-alist '((sh-mode . bash-ts-mode)))

;; creation and management of command aliases
(require 'em-alias)
(setq eshell-aliases-file "~/.emacs.d/eshell/alias")

;; terminal.c functions related to terminal devices
(setq ring-bell-function 'ignore)

;; Hooks
(add-hook 'eshell-mode-hook
	  (defun eshell-hook ()
	    (eshell/addpath (concat (getenv "HOME") "/.local/bin"))
	    (eshell/addpath (concat (getenv "HOME") "/bin"))))

;; Keybindings
(keymap-global-set "M-!" 'eshell-command)

(provide 'shell-conf)
;;; shell-conf.el ends here
