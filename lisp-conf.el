;;; lisp-conf.el --- lisp programming configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; lisp.el Lisp editing commands for Emacs
(setq delete-pair-blink-delay 0.25)

;; Paren Matching
(setq blink-matching-paren 'jump)

;; highlight matching paren
(require 'paren)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-style 'parenthesis)

;; Hooks

;; minor mode for editing parentheses
(require 'paredit)
(add-hook 'paredit-mode-hook
          (defun paredit-hook ()
            (keymap-unset paredit-mode-map "M-r" t)
            (keymap-local-set "M-[" #'paredit-wrap-square)
            (keymap-local-set "M-{" #'paredit-wrap-curly)
            (keymap-local-set "{" #'paredit-open-curly)))

(add-hook 'emacs-lisp-mode-hook
          (defun elisp-hook ()
            (lisp-hook)
            (keymap-local-set "C-c x" #'ielm-window)))

(add-hook 'ielm-mode-hook
          (defun ielm-hook ()
            (lisp-hook)
            (keymap-local-set "C-j" #'newline)
            (keymap-local-set "<return>" #'ielm-return)))

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

(defun copy-comment-sexp ()
  "Copy sexp at point to the kill ring and comment it out."
  (interactive)
  (let ((beg (point))
        (end (progn
               (forward-sexp)
               (point))))
    (copy-region-as-kill beg end)
    (comment-region beg end)))

(provide 'lisp-conf)
;;; lisp-conf.el ends here
