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
            (keymap-local-set "L" #'vc-print-root-log-fill-window)))

;; Functions
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
(keymap-global-set "C-x v c" 'vc-dir-config)

(provide 'vc-conf)
;;; vc-conf.el ends here
