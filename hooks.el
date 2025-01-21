;;; hooks.el --- Mode hooks -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-hook 'find-function-after-hook #'view-mode-enter)
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(add-hook 'eshell-mode-hook
	  (defun eshell-hook ()
	    (eshell/addpath (concat (getenv "HOME") "/.local/bin"))
	    (eshell/addpath (concat (getenv "HOME") "/bin"))))

(add-hook 'markdown-mode-hook
	  (defun markdown-hook ()
	    (keymap-local-set "C-S-p" #'markdown-previous-visible-heading)
	    (keymap-local-set "C-S-n" #'markdown-next-visible-heading)))

(add-hook 'sh-mode-hook
	  (defun sh-hook ()
	    (flycheck-mode)))

(add-hook 'prog-mode-hook
	  (defun prog-hook ()
	    (display-line-numbers-mode +1)
            (goggles-mode)
            (keymap-local-set "C-c c" #'compile)))

(add-hook 'text-mode-hook
          (defun text-hook ()
            (display-line-numbers-mode +1)))

(add-hook 'outline-minor-mode-hook
          (defun outline-hook ()
            (outline-cycle-buffer)))

(provide 'hooks)
;;; hooks.el ends here.
