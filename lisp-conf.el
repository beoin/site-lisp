;;; lisp-conf.el --- lisp programming configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Vars
(add-to-list 'auto-mode-alist '("\\.scrbl\\'" . racket-hash-lang-mode))
(defvar inferior-lisp-program "/usr/bin/sbcl")
(defvar delete-pair-blink-delay 0.25)

;; Paren Matching
(setq blink-matching-paren 'jump)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-style 'parenthesis)

;; Hooks
(add-hook 'racket-mode-hook
          (defun racket-hook ()
            (lisp-hook)))

(add-hook 'racket-hash-lang-mode-hook
          (defun racket-hash-lang-hook ()
            (setq-local racket-xp-add-binding-faces t)))

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
            (keymap-local-set "C-c x" #'ielm-window)))

(add-hook 'ielm-mode-hook
          (defun ielm-hook ()
            (lisp-hook)
            (keymap-local-set "C-j" #'newline)))

(add-hook 'sly-mode-hook
          (defun sly-hook ()
            (lisp-hook)))

(add-hook 'sly-mrepl-mode-hook
          (defun sly-repl-hook ()
            (keymap-local-set "<return>" #'sly-mrepl-return)))

(add-hook 'eval-expression-minibuffer-setup-hook
          (defun eval-hook ()
            (paredit-mode)))

(defun lisp-hook ()
  (paredit-mode)
  (rainbow-delimiters-mode)
  (prettify-symbols-mode)
  (keymap-local-set "<f7>" #'imenu))


(provide 'lisp-conf)
;;; lisp-conf.el ends here
