;;; lisp-conf.el --- lisp programming configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Vars
(add-to-list 'auto-mode-alist '("\\.scrbl\\'" . racket-hash-lang-mode))

;; lisp.el Lisp editing commands for Emacs
(setq delete-pair-blink-delay 0.25)

;; an inferior-lisp mode
(require 'inf-lisp)
(setq inferior-lisp-program "/usr/bin/sbcl")

;; Paren Matching
(setq blink-matching-paren 'jump)

;; highlight matching paren
(require 'paren)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-style 'parenthesis)

;; Hooks
(add-hook 'racket-mode-hook
          (defun racket-hook ()
            (lisp-hook)))

(add-hook 'racket-hash-lang-mode-hook
          (defun racket-hash-lang-hook ()
            (setq-local racket-xp-add-binding-faces t)))

;; minor mode for editing parentheses
(require 'paredit)
(add-hook 'paredit-mode-hook
          (defun paredit-hook ()
            (keymap-unset paredit-mode-map "M-r" t)
            (keymap-local-set "M-[" #'paredit-wrap-square)
            (keymap-local-set "M-{" #'paredit-wrap-curly)
            (keymap-local-set "{" #'paredit-open-curly)))

(add-hook 'clojure-mode-hook
          (defun clojure-hook ()
            (lisp-hook)))

(add-hook 'emacs-lisp-mode-hook
          (defun elisp-hook ()
            (lisp-hook)
            (keymap-local-set "C-c x" #'ielm-window)
            (keymap-local-set "C-a" 'crux-move-beginning-of-line)))

(add-hook 'ielm-mode-hook
          (defun ielm-hook ()
            (lisp-hook)
            (keymap-local-set "C-j" #'newline)
            (keymap-local-set "<return>" #'ielm-return)))

(add-hook 'sly-mode-hook
          (defun sly-hook ()
            (lisp-hook)))

(add-hook 'sly-mrepl-mode-hook
          (defun sly-repl-hook ()
            (keymap-local-set "<return>" #'sly-mrepl-return)))

(add-hook 'eval-expression-minibuffer-setup-hook
          (defun eval-hook ()
            (paredit-mode)
            (keymap-local-set "<return>" #'read--expression-try-read)))

(defun lisp-hook ()
  "Hook applied across modes."
  (paredit-mode)
  (rainbow-delimiters-mode)
  (prettify-symbols-mode)
  (lisp-extra-font-lock-mode)
  (keymap-local-set "<f7>" #'imenu))

;; Functions
(defun ielm-window ()
  "Launch an ielm session in other window."
  (interactive)
  (if (equal (one-window-p) t)
      (progn
        (split-window-horizontally)
        (other-window 1)
        (ielm))
    (progn
      (delete-other-windows)
      (ielm-window))))

(provide 'lisp-conf)
;;; lisp-conf.el ends here
