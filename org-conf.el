;; org-conf.el --- org-mode config.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Vars
(defvar org-agenda-files "~/Org/agenda-files.org")
(defvar org-directory "~/Org")
(defvar org-default-notes-file (concat org-directory "/capture_notes.org"))
(defvar org-use-tag-inheritance nil)
(defvar org-hide-emphasis-markers t)
(defvar org-startup-folded t)
(global-auto-revert-mode t)
(defvar org-rainbow-tags-hash-start-index 10)
(defvar org-return-follows-link t)
(defvar org-link-descriptive t)
(defvar org-cycle-separator-lines 0)
(defvar org-ellipsis "…")
(defvar org-fontify-quote-and-verse-blocks t)
(defvar org-pretty-entities nil)
(defvar org-goto-interface 'outline-path-completion)

;; Hooks
(add-hook 'org-mode-hook
	  (defun org-hook ()
	    (keymap-local-set "C-S-n" #'org-next-visible-heading)
	    (keymap-local-set "C-S-p" (lambda () (interactive) (org-next-visible-heading -1)))
	    (keymap-local-set "M-l" #'downcase-word-start)
	    (keymap-local-set "M-u" #'upcase-word-start)
	    (keymap-local-set "C-j" #'open-line-below-point)
	    (keymap-local-set "C-u C-k" #'kill-line-backward)
	    (keymap-local-set "C-u <tab>" #'org-overview)
	    (keymap-local-set "<f7>" #'imenu)
            (flyspell-mode +1)
	    (display-line-numbers-mode -1)
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
