;;; buffers-conf.el --- buffer related configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; GNU Emacs window commands aside from those written in C
(require 'window)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)
(setq display-buffer-alist nil)

;; Unique buffer names dependent on file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-trailing-separator-p 't)

;; Peruse file or buffer without editing
(require 'view)
(defvar view-scroll-auto-exit t)

;; Display generation from window structure and buffer text.
(setq message-log-max 250)

;; Buffer manipulation primitives for GNU Emacs.
(setq kill-buffer-delete-auto-save-files t)

;; Operate on buffers like dired
(require 'ibuffer)
(defvar ibuffer-movement-cycle nil)
(defvar ibuffer-old-time 24)
(defvar ibuffer-default-sorting-mode 'alphabetic)
(defvar ibuffer-expert nil)

;; Display available keybindings in popup
(require 'which-key)
(which-key-mode)
(which-key-setup-minibuffer)

;; Minibuffer
(minibuffer-electric-default-mode)
(setq use-short-answers t)
(setq enable-recursive-minibuffers t)
(setq kill-do-not-save-duplicates t)
(setq max-mini-window-height 10.00)
(setq minibuffer-follows-selected-frame t)
(setq file-name-shadow-properties '(invisible t intangible t face file-name-shadow field shadow))
(setq resize-mini-windows 'grow-only)

;; minibuffer and completion functions
(require 'minibuffer)
(setq insert-default-directory t)
(setq minibuffer-visible-completions t)

;; minibuffer completion incremental feedback
(require 'icomplete)
(fido-mode)

;; Functions
(defun delete-this-buffer-and-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun find-view-file ()
  "With prefix \"view-file\" otherwise \"find-file\"."
  (interactive)
  (if current-prefix-arg
      (command-execute #'view-file)
      (command-execute #'find-file)))

;;Keybindings
(keymap-global-set "C-x B" 'ibuffer-list-buffers)
(keymap-global-set "C-x C-b" 'ibuffer)

(provide 'buffers-conf)
;;; buffers-conf.el ends here
