;;; buffers-conf.el --- buffer related configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; window commands aside from those written in C
(require 'window)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions 't)
(setq display-buffer-alist nil)

;; unique buffer names dependent on file name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-trailing-separator-p 't)

;; peruse file or buffer without editing
(require 'view)
(setq view-scroll-auto-exit 't)

;; buffer.c buffer manipulation primitives
(setq kill-buffer-delete-auto-save-files 't)
(setq case-fold-search 't)
(setq cursor-type 'box)
(setq-default word-wrap 't)
(setq-default truncate-lines 't)
(setq-default fill-column 80)
(setq-default tab-width 8)
(setq-default indicate-empty-lines 't)

;; operate on buffers like dired
(require 'ibuffer)
(setq ibuffer-movement-cycle nil)
(setq ibuffer-default-sorting-mode 'recency)
(setq ibuffer-expert nil)
(setq ibuffer-use-other-window nil)

;; extensions for ibuffer
(require 'ibuf-ext)
(setq ibuffer-old-time 24)

;; display available keybindings in popup
(require 'which-key)
(which-key-mode)
(which-key-setup-minibuffer)

;; highlight shadowed part of read-file-name input text
(require 'rfn-eshadow)
(setq file-name-shadow-properties '(invisible t intangible t face file-name-shadow field shadow))

;; save minibuffer history
(require 'savehist)
(savehist-mode)

;; minibuf.c minibuffer input and completion.
(setq minibuffer-follows-selected-frame 't)
(setq enable-recursive-minibuffers 't)
(setq history-delete-duplicates 't)

;; minibuffer and completion functions
(require 'minibuffer)
(setq insert-default-directory 't)
(setq minibuffer-visible-completions 't)
(setq completions-format 'horizontal)
(setq completion-auto-select 'second-tab)
(setq read-file-name-completion-ignore-case nil)
(setq minibuffer-default-prompt-format " [%s]")
(setq completions-header-format
      (propertize "%s matches:\n" 'face 'shadow))

;; Only show defaults in prompts when applicable
(require 'minibuf-eldef)
(minibuffer-electric-default-mode)

;; Indicate minibuffer-depth in prompt
(require 'mb-depth)
(minibuffer-depth-indicate-mode)

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

;; Keybindings
(keymap-global-set "C-x C-b" 'ibuffer)
(keymap-global-set "C-c S" 'scratch-buffer)
(keymap-global-set "C-c M" 'messages-buffer)

(provide 'buffers-conf)
;;; buffers-conf.el ends here
