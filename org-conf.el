;; org-conf.el --- org-mode config.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; compatibility code for older emacsen
(require 'org-compat)
(setq org-imenu-depth 2)

;; visibility cycling of org entries
(require 'org-cycle)
(setq org-cycle-hide-drawer-startup t)
(setq org-hide-block-startup t)
(setq org-cycle-separator-lines 0)
(setq org-cycle-hide-block-startup t)

;; babel. working with code blocks
(require 'ob-core)
(setq org-babel-uppercase-example-markers t)
(setq org-babel-hash-show-time t)

;; babel functions for common lisp
(require 'ob-lisp)
(setq org-babel-lisp-eval-fn 'sly-eval)

;; colorize org tags automatically
(require 'org-rainbow-tags)
(setq org-rainbow-tags-hash-start-index 10)

;; key bindings for org mode
(require 'org-keys)
(setq org-return-follows-link nil)

;; org links library
(require 'ol)
(setq org-link-descriptive t)

;; face definitions
(require 'org-faces)
(setq org-fontify-quote-and-verse-blocks t)

;; fast navigation in an org buffer
(require 'org-goto)
(setq org-goto-interface 'outline-path-completion)

;; source code examples in org
(require 'org-src)
(setq org-src-window-setup 'current-window)

;; outline-based notes management and organizer
(require 'org)
(setq org-startup-with-inline-images nil)
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
	    (keymap-local-set "C-S-p" #'org-previous-visible-heading)
	    (keymap-local-set "C-j" #'open-line-below-point)
	    (keymap-local-set "C-<tab>" #'org-cycle-overview)
	    (keymap-local-set "<f7>" #'imenu)
            (keymap-local-set "C-c h" #'org-toggle-heading)
            (keymap-local-set "C-c C-d" #'org-do-demote)
            (keymap-local-set "C-c C-p" #'org-do-promote)
            (keymap-local-set "<escape> :" #'org-emphasize-colon)
            (keymap-local-set "<escape> ;" #'org-emphasize-code)
            (keymap-local-set "M-<tab>" #'complete-symbol)
            (keymap-local-set "<escape> o s a" 'org-sort-alpha)
            (keymap-local-set "<escape> o d d" 'org-do-demote)
            (keymap-local-set "<escape> o d p" 'org-do-promote)
            (flyspell-mode +1)
            (org-rainbow-tags-mode)
            (electric-pair-mode -1)
            (goggles-mode)
            (variable-pitch-mode)
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

(defun org-sort-alpha ()
  "Quickly sort the top-level an org document alphabetically."
  (interactive)
  (let ((curr (point)))
    (if (progn
          (goto-char 0)
          (current-line-empty-p))
        (org-sort-entries nil ?a)
      (progn (open-line 1)
             (org-sort-entries nil ?a)
             (goto-char 0)
             (kill-line)))
    (goto-char curr)
    (org-overview)))

(defun org-emphasize-code ()
  "Emphasise the symbol at point as \"code\"."
  (interactive)
  (let ((curr (point))
        (end (goto-char (re-search-forward "[ \t]+\\|\n" nil nil nil))))
    (goto-char curr)
    (insert "~")
    (goto-char end)
    (insert "~")))

(defun org-emphasize-colon ()
  "Emphasise from the start of the current line until the first colon symbol."
  (interactive)
  (beginning-of-line)
  (let ((start (1- (re-search-forward "[[:alnum:]]" (line-end-position) t 1)))
        (end (re-search-forward "[:\t]+\\|\n" (line-end-position) t 1)))
    (or end (error "Error: colon not found"))
    (goto-char start)
    (insert "=")
    (goto-char end)
    (insert "=")))

(defun org-line-folded-p ()
  "Non-nil if line at point is in folded state."
  (org-fold-core-folded-p (line-end-position) nil))

(provide 'org-conf)
;;; org-conf.el ends here
