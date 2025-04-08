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

;; revert buffers when files on disk change
(require 'autorevert)
(global-auto-revert-mode)

;; Vars
(setq delete-by-moving-to-trash t)
(defvar archive-visit-single-files t)

;; Custom File
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Functions
(defun show-file-name ()
  "Echo the full path of the current file."
  (interactive)
  (message (buffer-file-name)))

(defun grep-config-symbol-at-point ()
  "Recursively grep config for the symbol at point."
  (interactive)
  (rgrep (thing-at-point 'symbol) "*" config-files))

(defun grep-config (arg)
  "Recursivly grep config with ARG."
  (interactive "sGrep for: ")
  (rgrep arg "*" config-files))

(defun find-view-file ()
  "With prefix \"view-file\" otherwise \"find-file\"."
  (interactive)
  (if current-prefix-arg
      (command-execute #'view-file)
      (command-execute #'find-file)))

;; Keybindings
(keymap-global-set "C-c t" (defun todo()(interactive)(find-file "~/Org/todo.org")))
(keymap-global-set "C-x C-f" 'find-view-file)

(provide 'files-conf)
;;; files-conf.el ends here
