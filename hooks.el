;;; Commentary:
;;; mode hooks.

;;; Code:
(add-hook 'go-mode-hook 'lsp-deferred)
;;(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook 
	  (defun go-hook ()
	    (keymap-local-set "C-c f" #'gofmt)
	    (keymap-local-set "C-<tab>" #'company-complete)
	    ))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(add-hook 'find-function-after-hook #'view-mode-enter)

(add-hook 'paredit-mode-hook (defun paredit-hook () (keymap-unset paredit-mode-map "M-r" t)))
(add-hook 'emacs-lisp-mode-hook (defun elisp-hook ()(lisp-hook)))
(add-hook 'clojure-mode-hook (defun clojure-hook () (lisp-hook)))

(add-hook 'hare-mode-hook
	  (defun hare-hook ()
	    (keymap-local-set "C-M-a" #'hare-mode-beginning-of-defun)
	    (keymap-local-set "C-M-e" #'hare-mode-end-of-defun)))

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
	    (keymap-local-set "C-j" #'newline-above-below-point)
	    (flyspell-mode +1)
	    (display-line-numbers-mode -1)
	    (setq buffer-face-mode-face '(:family "Cantarell" :height 140 :width regular))
	    (buffer-face-mode)
	    ;(electric-pair-local-mode -1)
	    ))
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
	    (flymake-mode 1)
	    (flymake-shellcheck-load)))

(add-hook 'dired-mode-hook
	  (defun dired-hook ()
	    (keymap-local-set "c" #'dired-create-empty-file)
	    (display-line-numbers-mode t)
	    (dired-omit-mode t)))

(add-hook 'zig-mode-hook
	  (defun zig-hook ()
	    (keymap-local-set "<f9>" #'zig-compile)))

(add-hook 'prog-mode-hook
	  (defun prog-hook ()
	    (display-line-numbers-mode +1)))

(provide 'hooks)

;;; hooks.el ends here.
