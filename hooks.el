;;; hooks.el --- Mode hooks -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Imports
(require 'outline)
(require 'markdown-mode)

;; Hooks
(add-hook 'find-function-after-hook #'view-mode-enter)
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'outline-mode-hook
          (defun outline-hook ()
            (outline-show-only-headings)))

(add-hook 'emacs-startup-hook
          (defun startup-hook ()
            (setq gc-cons-threshold (* 50 1024 1024))))

(add-hook 'odin-mode-hook
          (defun odin-hook ()
            (electric-pair-mode)))

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

(add-hook 'outline-minor-mode-hook
          (defun outline-hook ()
            (outline-cycle-buffer 1)))

(add-hook 'emacs-news-view-mode-hook
          (defun emacs-news-hook ()
            (keymap-local-set "C-S-n" #'outline-next-visible-heading)
            (keymap-local-set "C-S-p" #'outline-previous-visible-heading)))

(provide 'hooks)
;;; hooks.el ends here.
