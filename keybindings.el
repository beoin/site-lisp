;;; keybindings.el --- Global keybindings -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(keymap-global-unset "C-x m")  ;; compose-mail
(keymap-global-unset "C-x t")  ;; previous-window-any-frame
(keymap-global-unset "C-z")    ;; suspend-frame
(keymap-global-unset "C-x v x");; vc-delete-file
(keymap-global-unset "C-x f")  ;; set-fill-column
(keymap-global-unset "C-x C-q");; read-only-mode
(keymap-global-unset "<XF86WakeUp>")
(keymap-global-unset "<print>")

;; Escape
(keymap-global-set "<escape> w s" 'toggle-window-split)
(keymap-global-set "<escape> z c" 'zap-to-char)
(keymap-global-set "<escape> z s" 'zap-up-to-string)
(keymap-global-set "<escape> z u" 'zap-up-to-char)
(keymap-global-set "<escape> ." 'delete-space-period-line-end)
(keymap-global-set "<escape> <escape>" 'switch-to-minibuffer)

;; C-c
(keymap-global-set "C-c m" 'move-to-line-middle)
(keymap-global-set "C-c k" 'delete-current-line)
(keymap-global-set "C-c r" 'replace-char)
(keymap-global-set "C-x t" 'previous-window-any-frame)
(keymap-global-set "C-c s" 'status)
(keymap-global-set "C-c v" 'view-mode)
(keymap-global-set "C-c l" 'comment-or-uncomment-region)
(keymap-global-set "C-c w" 'lookup-word-at-point)
(keymap-global-set "C-c W" 'google-define)
(keymap-global-set "C-c U" 'search-github)
(keymap-global-set "C-c u" 'search-github-ext)
(keymap-global-set "C-c P" 'search-wikipedia)
(keymap-global-set "C-c g" 'search-duckduckgo)
(keymap-global-set "C-c G" 'search-google)
(keymap-global-set "C-c y" 'search-youtube)
(keymap-global-set "C-c B" 'search-google-books)
(keymap-global-set "C-c p" 'search-archwiki)
(keymap-global-set "C-c d" 'duplicate-dwim)

;; C-x
(keymap-global-set "C-x C-r" 'restart-emacs)
(keymap-global-set "C-x e" 'emacs-version)

;; Meta
(keymap-global-set "M-<backspace>" 'backward-delete-word)
(keymap-global-set "M-o" 'other-window)
(keymap-global-set "M-u" 'upcase-word-start)
(keymap-global-set "M-c" 'capitalise-word)
(keymap-global-set "M-z" 'quick-zap-up-to-char)
(keymap-global-set "M-l" 'downcase-word-start)

;; Shift
(keymap-global-set "S-<right>" 'windmove-right)
(keymap-global-set "S-<left>" 'windmove-left)
(keymap-global-set "S-<up>" 'windmove-up)
(keymap-global-set "S-<down>" 'windmove-down)

(provide 'keybindings)
;;; keybindings.el ends here
