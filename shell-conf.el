;;; shell-conf.el --- Shell configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Imports
(require 'flymake-shellcheck)

;; Vars
(defvar eshell-aliases-file "~/.emacs.d/eshell/alias")
(defvar flycheck-sh-shellcheck-executable "~/.local/bin/shellcheck")
(setq major-mode-remap-alist '((sh-mode . bash-ts-mode)))
(setq flymake-shellcheck-program "~/.local/bin/shellcheck")

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
