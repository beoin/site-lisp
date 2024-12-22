;;; hooks.el --- Mode hooks -*- lexical-binding: t; -*-

;;; Commentary:
;;; mode hooks.

;;; Code:

(add-hook 'find-function-after-hook #'view-mode-enter)
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(add-hook 'paredit-mode-hook (defun paredit-hook ()
                               (keymap-unset paredit-mode-map "M-r" t)
                               (keymap-local-set "M-[" #'paredit-wrap-square)
                               (keymap-local-set "M-{" #'paredit-wrap-curly)))

(add-hook 'clojure-mode-hook (defun clojure-hook ()
                               (lisp-hook)))

(add-hook 'emacs-lisp-mode-hook
          (defun elisp-hook ()
            (lisp-hook)
            (keymap-local-set "C-c x" #'ielm-window)))

(defun lisp-hook ()
  (paredit-mode)
  (rainbow-delimiters-mode)
  (prettify-symbols-mode))

(add-hook 'eshell-mode-hook
	  (defun eshell-hook ()
	    (eshell/addpath (concat (getenv "HOME") "/.local/bin"))
	    (eshell/addpath (concat (getenv "HOME") "/bin"))))

(add-hook 'ielm-mode-hook
          (defun ielm-hook ()
            (lisp-hook)
            (keymap-local-set "C-j" #'newline)))

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
            (goggles-mode)))


(provide 'hooks)
;;; hooks.el ends here.
