;;; buffers-conf.el --- buffer related configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Imports
(require 'vertico)
(require 'embark)

;; Vars
(setq view-read-only t)
(defvar view-scroll-auto-exit t)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)
(setq display-buffer-alist nil)
(setq message-log-max 250)

;; Ibuffer
(defvar ibuffer-movement-cycle nil)
(defvar ibuffer-old-time 24)
(defvar ibuffer-default-sorting-mode 'alphabetic)
(defvar ibuffer-expert nil)

;; Minibuffer
(vertico-mode +1)
(marginalia-mode +1)
(which-key-mode)
(which-key-setup-minibuffer)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))
(setq use-short-answers t)
(setq enable-recursive-minibuffers t)
(setq kill-do-not-save-duplicates t)
(setq vertico-count 15)
(setq max-mini-window-height 10.00)
(setq embark-confirm-act-all t)
(setq minibuffer-visible-completions t)
(setq minibuffer-follows-selected-frame t)
(setq file-name-shadow-properties '(invisible t intangible t face file-name-shadow field shadow))
(setq insert-default-directory t)
(setq resize-mini-windows 'grow-only)

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

;;Keybindings
(keymap-global-set "C-x B" 'ibuffer-list-buffers)
(keymap-global-set "C-." 'embark-act)

(provide 'buffers-conf)
;;; buffers-conf.el ends here
