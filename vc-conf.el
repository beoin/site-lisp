;;; vc-conf.el --- version control configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; resident support for version-control
(require 'vc-hooks)
(setq vc-follow-symlinks nil)

;; Hooks
(add-hook 'vc-dir-mode-hook
          (defun vc-mode-hook ()
            (keymap-local-set "v" #'vc-next-action-window-below)
            (keymap-local-set "L" #'vc-print-root-log-fill-window)
            (keymap-local-set "r" #'vc-refresh-state)))

(add-hook 'diff-mode-hook
          (defun diff-hook ()
            (keymap-local-unset "M-o")
            (keymap-local-set "s" #'diff-goto-source-view-file)))

;; Functions
(defun diff-goto-source-view-file ()
  "Jump to the corresponding source line under \"view-mode\"."
  (interactive)
  (diff-goto-source)
  (view-mode))

(defun vc-print-root-log-fill-window ()
  "Fill the current window when \"vc-print-root-log\" is called."
  (interactive)
  (vc-print-root-log)
  (delete-other-windows))

(defun vc-next-action-window-below ()
  "Do the next logical vc operation in the current window split below."
  (interactive)
  (split-window-below)
  (vc-next-action nil))

(defun vc-dir-config ()
  "Execute the function \"vc-dir\" within the Emacs config."
  (interactive)
  (vc-dir config-files))

;; Keybindings
(keymap-global-set "C-x v c" 'vc-dir-root)

(provide 'vc-conf)
;;; vc-conf.el ends here
