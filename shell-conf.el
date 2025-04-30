;;; shell-conf.el --- Shell configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(setq major-mode-remap-alist '((sh-mode . bash-ts-mode)))

;; creation and management of command aliases
(require 'em-alias)
(setq eshell-aliases-file "~/.emacs.d/eshell/alias")

;; on-the-fly syntax checking
(require 'flycheck)
(setq flycheck-sh-shellcheck-executable "~/.local/bin/shellcheck")

;; a bash/sh Flymake backend powered by ShellCheck
(require 'flymake-shellcheck)
(setq flymake-shellcheck-program "~/.local/bin/shellcheck")

;; terminal.c functions related to terminal devices
(setq ring-bell-function 'ignore)

;; Hooks
(add-hook 'eshell-mode-hook
	  (defun eshell-hook ()
	    (eshell/addpath (concat (getenv "HOME") "/.local/bin"))
	    (eshell/addpath (concat (getenv "HOME") "/bin"))))

(add-hook 'sh-mode-hook
	  (defun sh-hook ()
	    (flycheck-mode)))

;; Keybindings
(keymap-global-set "M-!" 'eshell-command)

(provide 'shell-conf)
;;; shell-conf.el ends here
