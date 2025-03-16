;;; hooks.el --- Mode hooks -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-hook 'find-function-after-hook #'view-mode-enter)
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(require 'markdown-mode)
(add-hook 'markdown-mode-hook
	  (defun markdown-hook ()
	    (keymap-local-set "C-S-p" #'markdown-previous-visible-heading)
	    (keymap-local-set "C-S-n" #'markdown-next-visible-heading)))

(add-hook 'prog-mode-hook
	  (defun prog-hook ()
	    (display-line-numbers-mode +1)
            (goggles-mode)
            (hl-line-mode)
            (flycheck-mode)
            (keymap-local-set "C-c c" #'compile)))

(add-hook 'text-mode-hook
          (defun text-hook ()))

(add-hook 'outline-minor-mode-hook
          (defun outline-hook ()
            (outline-cycle-buffer 1)))

(add-hook 'diff-mode-hook
          (defun diff-hook ()
            (keymap-local-unset "M-o")))

(require 'outline)
(add-hook 'emacs-news-view-mode-hook
          (defun emacs-news-hook ()
            (keymap-local-set "C-S-n" #'outline-next-visible-heading)
            (keymap-local-set "C-S-p" #'outline-previous-visible-heading)))

(provide 'hooks)
;;; hooks.el ends here.
