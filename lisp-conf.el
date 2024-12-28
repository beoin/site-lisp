;;; lisp-conf.el --- lisp programming configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Vars
(add-to-list 'auto-mode-alist '("\\.scrbl\\'" . racket-hash-lang-mode))

;; Hooks
(add-hook 'racket-hash-lang-mode-hook
          (defun racket-hash-lang-hook ()
            (setq-local racket-xp-add-binding-faces t)))

(add-hook 'paredit-mode-hook
          (defun paredit-hook ()
            (keymap-unset paredit-mode-map "M-r" t)
            (keymap-local-set "M-[" #'paredit-wrap-square)
            (keymap-local-set "M-{" #'paredit-wrap-curly)))

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

(defun lisp-hook ()
  (paredit-mode)
  (rainbow-delimiters-mode)
  (prettify-symbols-mode))

(provide 'lisp-conf)
;;; lisp-conf.el ends here
