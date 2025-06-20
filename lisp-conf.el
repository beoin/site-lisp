;;; lisp-conf.el --- lisp programming configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; interaction mode for Emacs Lisp
(require 'ielm)
(setq ielm-noisy nil)

;; lisp.el Lisp editing commands for Emacs
;; (require 'lisp) not provided
(setq delete-pair-blink-delay 0.25)

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
            (keymap-local-set "{" #'paredit-open-curly)
            (keymap-local-set "<" #'paredit-open-angled)
            (keymap-local-set "C-M-w" #'paredit-backward-up)))

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
  "Comment sexp at point and add it to the kill ring."
  (interactive)
  (let ((beg (point))
        (end (progn
               (forward-sexp)
               (point))))
    (copy-region-as-kill beg end)
    (comment-region beg end)))

(defun copy-comment-yank-sexp ()
  "Comment sexp at point yank it to a new line below."
  (interactive)
  (copy-comment-sexp)
  (newline 2)
  (yank))


(provide 'lisp-conf)
;;; lisp-conf.el ends here
