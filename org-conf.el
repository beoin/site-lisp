;; org-conf.el --- org-mode config.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Visibility cycling of Org entries.
(require 'org-cycle)
(setq org-cycle-hide-drawer-startup t)
(setq org-hide-block-startup t)
(setq org-cycle-separator-lines 0)
(setq org-cycle-hide-block-startup t)

;; Babel. Working with Code Blocks.
(require 'ob-core)
(setq org-babel-uppercase-example-markers t)
(setq org-babel-hash-show-time t)

;; Babel Functions for Common Lisp
(require 'ob-lisp)
(setq org-babel-lisp-eval-fn 'sly-eval)

;; Colorize org tags automatically
(require 'org-rainbow-tags)
(setq org-rainbow-tags-hash-start-index 10)

;; Key bindings for Org mode
(require 'org-keys)
(setq org-return-follows-link nil)

;; Org links library
(require 'ol)
(setq org-link-descriptive t)

;; Face definitions
(require 'org-faces)
(setq org-fontify-quote-and-verse-blocks t)

;; Fast navigation in an Org buffer
(require 'org-goto)
(setq org-goto-interface 'outline-path-completion)

;; Source code examples in Org
(require 'org-src)
(setq org-src-window-setup 'current-window)

;; Outline-based notes management and organizer
(require 'org)
(setq org-startup-with-inline-images t)
(setq org-startup-folded 'fold)
(setq org-hide-emphasis-markers t)
(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-ctrl-k-protect-subtree 'query)
(setq org-agenda-files "~/Org/agenda-files.org")
(setq org-directory "~/Org")
(setq org-default-notes-file (concat org-directory "/capture_notes.org"))
(setq org-use-tag-inheritance nil)
(setq org-ellipsis "…")
(setq org-pretty-entities nil)
(setq org-M-RET-may-split-line '((headline . nil) (item . t) (table . t)))
(setq org-loop-over-headlines-in-active-region t)

;; Hooks
(add-hook 'org-mode-hook
	  (defun org-hook ()
	    (keymap-local-set "C-S-n" #'org-next-visible-heading)
	    (keymap-local-set "C-S-p" (lambda () (interactive) (org-next-visible-heading -1)))
	    (keymap-local-set "C-j" #'open-line-below-point)
	    (keymap-local-set "C-<tab>" #'org-cycle-overview)
	    (keymap-local-set "<f7>" #'imenu)
            (keymap-local-set "C-c h" #'org-toggle-heading)
            (keymap-local-set "C-c C-d" #'org-do-demote)
            (keymap-local-set "C-c C-p" #'org-do-promote)
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

;; Functions
(defun org-news ()
    "Open the \"org-mode\" news file."
    (interactive)
    (find-file (concat data-directory "ORG-NEWS")))

(provide 'org-conf)
;;; org-conf.el ends here
