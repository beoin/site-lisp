;;; hooks.el --- Mode hooks -*- lexical-binding: t; -*-

;;; Commentary:
;;; mode hooks.

;;; Code:

(add-hook 'zig-ts-mode-hook
	  (defun zig-hook ()
	    (keymap-local-set "<f9>" #'zig-compile)
	    (keymap-local-set "<f7>" #'goto-line)))

(add-hook 'find-function-after-hook #'view-mode-enter)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(add-hook 'paredit-mode-hook (defun paredit-hook () (keymap-unset paredit-mode-map "M-r" t)))
(add-hook 'clojure-mode-hook (defun clojure-hook () (lisp-hook)))
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
          (defun ielm-hook () (keymap-local-set "C-j" #'newline)))

(add-hook 'org-mode-hook
	  (defun org-hook ()
	    (keymap-local-set "C-S-n" #'org-next-visible-heading)
	    (keymap-local-set "C-S-p" (lambda () (interactive) (org-next-visible-heading -1)))
	    (keymap-local-set "M-l" #'downcase-word-start)
	    (keymap-local-set "M-u" #'upcase-word-start)
	    (keymap-local-set "C-j" #'open-line-below-point)
	    (keymap-local-set "C-u C-k" #'kill-line-backward)
	    (keymap-local-set "C-u <tab>" #'org-overview)
	    (flyspell-mode +1)
	    (display-line-numbers-mode -1)
	    (setq buffer-face-mode-face '(:family "Cantarell" :height 140))
	    (buffer-face-mode)
            (org-rainbow-tags-mode)
            (goggles-mode)))

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(add-hook 'markdown-mode-hook
	  (defun markdown-hook ()
	    (keymap-local-set "C-S-p" #'markdown-previous-visible-heading)
	    (keymap-local-set "C-S-n" #'markdown-next-visible-heading)))

(add-hook 'sh-mode-hook
	  (defun sh-hook ()
	    (flycheck-mode)))

(add-hook 'dired-mode-hook
	  (defun dired-hook ()
	    (keymap-local-set "c" #'dired-create-empty-file)
	    (display-line-numbers-mode t)
	    (dired-omit-mode t)
	    (diredfl-global-mode t)))

(add-hook 'prog-mode-hook
	  (defun prog-hook ()
	    (display-line-numbers-mode +1)
            (goggles-mode)))


(provide 'hooks)
;;; hooks.el ends here.
