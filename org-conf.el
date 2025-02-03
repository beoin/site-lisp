;; org-conf.el --- org-mode config.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Vars
(defvar org-cycle-separator-lines 0)
(defvar org-cycle-hide-block-startup t)
(defvar org-cycle-hide-drawer-startup t)
(defvar org-startup-with-inline-images t)
(defvar org-startup-folded t)
(defvar org-hide-emphasis-markers t)
(defvar org-hide-block-startup t)
(defvar org-babel-lisp-eval-fn 'sly-eval)
(defvar org-babel-uppercase-example-markers t)
(defvar org-ctrl-k-protect-subtree 'query)
(defvar org-agenda-files "~/Org/agenda-files.org")
(defvar org-directory "~/Org")
(defvar org-default-notes-file (concat org-directory "/capture_notes.org"))
(defvar org-use-tag-inheritance nil)
(defvar org-rainbow-tags-hash-start-index 10)
(defvar org-return-follows-link nil)
(defvar org-link-descriptive t)
(defvar org-ellipsis "…")
(defvar org-fontify-quote-and-verse-blocks t)
(defvar org-pretty-entities nil)
(defvar org-goto-interface 'outline-path-completion)

;; Hooks
(add-hook 'org-mode-hook
	  (defun org-hook ()
	    (keymap-local-set "C-S-n" #'org-next-visible-heading)
	    (keymap-local-set "C-S-p" (lambda () (interactive) (org-next-visible-heading -1)))
	    (keymap-local-set "C-j" #'open-line-below-point)
	    (keymap-local-set "C-<tab>" #'org-cycle-overview)
	    (keymap-local-set "<f7>" #'imenu)
            (keymap-local-set "C-c h" #'org-toggle-heading)
            (flyspell-mode +1)
	    (setq buffer-face-mode-face '(:family "Cantarell" :height 140))
	    (buffer-face-mode)
            (org-rainbow-tags-mode)
            (goggles-mode)
            (org-indent-mode)))

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(provide 'org-conf)
;;; org-conf.el ends here
