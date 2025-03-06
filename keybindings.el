;;; keybindings.el --- Global keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(keymap-global-unset "C-x m")
(keymap-global-unset "C-x t")
(keymap-global-unset "C-z")
(keymap-global-unset "<XF86WakeUp>")
(keymap-global-unset "<print>")
(keymap-global-set "C-x t" 'previous-window-any-frame)
(keymap-global-set "M-<backspace>" 'backward-delete-word)
(keymap-global-set "C-c m" 'move-to-line-middle)
(keymap-global-set "C-c k" 'delete-current-line)
(keymap-global-set "C-c r" 'replace-char)
(keymap-global-set "S-<right>" 'windmove-right)
(keymap-global-set "S-<left>" 'windmove-left)
(keymap-global-set "S-<up>" 'windmove-up)
(keymap-global-set "S-<down>" 'windmove-down)
(keymap-global-set "C-c s" 'status)
(keymap-global-set "C-c S" 'scratch-buffer)
(keymap-global-set "C-c M" 'messages-buffer)
(keymap-global-set "C-c t" (defun todo()(interactive)(find-file "~/Org/todo.org")))
(keymap-global-set "C-c v" 'view-mode)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "M-u" 'upcase-word-start)
(keymap-global-set "C-c l" 'comment-or-uncomment-region)
(keymap-global-set "C-c w" 'lookup-word-at-point)
(keymap-global-set "C-c W" 'google-define)
(keymap-global-set "C-x C-r" 'restart-emacs)
(keymap-global-set "C-x e" 'emacs-version)
(keymap-global-set "C-c i" 'crux-find-user-init-file)
(keymap-global-set "C-c d" 'crux-duplicate-current-line-or-region)
(keymap-global-set "M-c" 'capitalise-word)
(keymap-global-set "M-z" 'quick-zap-up-to-char)
(keymap-global-set "C-c U" 'search-github)
(keymap-global-set "C-c u" 'search-github-ext)
(keymap-global-set "C-c P" 'search-wikipedia)
(keymap-global-set "C-c g" 'search-duckduckgo)
(keymap-global-set "C-c G" 'search-google)
(keymap-global-set "C-c y" 'search-youtube)
(keymap-global-set "C-c B" 'search-google-books)
(keymap-global-set "C-c p" 'search-archwiki)
(keymap-global-set "M-l" 'downcase-word-start)

(provide 'keybindings)
;;; keybindings.el ends here
