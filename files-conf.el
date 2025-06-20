;;; files-conf.el --- file related configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; file input and output commands
(require 'files)
(setq large-file-warning-threshold 100000000)
(setq find-file-suppress-same-file-warnings t)
(setq backup-directory-alist '(("." . ".~")))
(setq version-control 'never)
(setq delete-old-versions t)
(setq view-read-only t)
(setq confirm-nonexistent-file-or-buffer 't)
(setq require-final-newline t)
(setq confirm-kill-emacs #'yes-or-no-p)

;; find file (or url) at point
(require 'ffap)
(setq ffap-machine-p-known 'reject)

;; revert buffers when files on disk change
(require 'autorevert)
(global-auto-revert-mode)

;; simple editing of archives
(require 'arc-mode)
(setq archive-visit-single-files t)

;; fileio.c file io
(setq delete-by-moving-to-trash t)

;; tools for customizing Emacs and Lisp packages
(require 'cus-edit)
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Functions
(defun show-file-name ()
  "Echo the full path of the current file."
  (interactive)
  (message (buffer-file-name)))

(defun grep-config-at-point ()
  "Recursively grep config for the symbol at point."
  (interactive)
  (rgrep (thing-at-point 'symbol) "*" config-files))

(defun grep-config (pattern)
  "Recursivly grep config with PATTERN."
  (interactive "sGrep for: ")
  (rgrep pattern "*" config-files))

(defun grep-src (pattern)
  "Recursively grep Emacs Lisp source files for PATTERN."
  (interactive "sGrep for: ")
  (rgrep pattern "*" (concat source-directory "lisp")))

(defun grep-src-at-point ()
  "Recursively grep Emacs Lisp source files for symbol at point."
  (interactive)
  (rgrep (thing-at-point 'symbol) "*"  (concat source-directory "lisp")))

(defun find-view-file ()
  "With prefix \"view-file\" otherwise \"find-file\"."
  (interactive)
  (if current-prefix-arg
      (command-execute #'view-file)
    (command-execute #'find-file)))

(defun touchx ()
  "Create an empty, executable file and load it into a buffer."
  (interactive)
  (let ((file-name (read-file-name "Create an executable file named: ")))
    (make-empty-file file-name)
    (chmod file-name #o755)
    (find-file file-name)))

(defun todo-file ()
  "Switch to todo.org. If prefix arg is supplied close all other windows."
  (interactive)
  (let ((todo "~/Org/todo.org"))
    (if current-prefix-arg
        (eval
         (find-file todo)
         (delete-other-windows))
      (find-file todo))))

;; Keybindings
(keymap-global-set "C-c t" 'todo-file)
(keymap-global-set "C-x C-f" 'find-view-file)

(provide 'files-conf)
;;; files-conf.el ends here
