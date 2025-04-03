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

(provide 'files-conf)
;;; files-conf.el ends here
