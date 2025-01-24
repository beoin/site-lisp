;;; keybindings.el --- Global keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(keymap-global-set "M-<backspace>" 'backward-delete-word)
(keymap-global-set "C-c m" 'move-to-line-middle)
(keymap-global-set "C-." 'embark-act)
(keymap-global-set "C-c k" 'delete-current-line)
(keymap-global-set "C-c r" 'replace-char)
(keymap-global-set "S-<right>" 'windmove-right)
(keymap-global-set "S-<left>" 'windmove-left)
(keymap-global-set "S-<up>" 'windmove-up)
(keymap-global-set "S-<down>" 'windmove-down)
(keymap-global-set "C-c i" 'crux-find-user-init-file)
(keymap-global-set "C-c s" 'status)
(keymap-global-set "C-c S" 'scratch-buffer)
(keymap-global-set "C-c t" (defun todo()(interactive)(find-file "~/Org/todo.org")))
(keymap-global-set "C-c v" 'view-mode)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "M-u" 'upcase-word-start)
(keymap-global-set "C-c l" 'comment-or-uncomment-region)
(keymap-global-set "C-c w" 'lookup-word-at-point)
(keymap-unset global-map "C-x d")
(global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "M-!") 'eshell-command)
(keymap-global-set "C-a" 'crux-move-beginning-of-line)
(keymap-global-set "M-c" 'capitalise-word)
(keymap-unset global-map "C-x m")
(keymap-global-set "C-x m e" 'eshell)
(keymap-global-set "C-x m s" 'shell)
(keymap-global-set "C-x m a" 'ansi-term)
(keymap-global-set "M-SPC" 'ispell-word)
(keymap-global-set "C-x C-r" 'restart-emacs)
(global-set-key (kbd "M-z") 'quick-zap-up-to-char)
(global-set-key (kbd "C-c U") 'search-github)
(global-set-key (kbd "C-c u") 'search-github-ext)
(global-set-key (kbd "C-c W") 'search-wikipedia)
(global-set-key (kbd "C-c g") 'search-duckduckgo)
(global-set-key (kbd "C-c G") 'search-google)
(global-set-key (kbd "C-c y") 'search-youtube)
(global-set-key (kbd "C-c B") 'search-book)
(global-set-key (kbd "C-c p") 'search-archwiki)
(keymap-global-set "C-x e" 'emacs-version)

(provide 'keybindings)
;;; keybindings.el ends here


