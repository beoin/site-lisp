;; dired-conf.el --- dired config  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Imports
(require 'dired)
(require 'dired-x)
(require 'dired-aux)
(require 'find-dired)

;; Vars
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-goa --group-directories-first")
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-create-destination-dirs 'ask)
(setq dired-ls-F-marks-symlinks t)
(setq dired-deletion-confirmer 'y-or-n-p)
(setq dired-dwim-target t)
(setq dired-clean-confirm-killing-deleted-buffers nil)
(setq dired-movement-style 'bounded)
(setq find-name-arg "-iname")
(setq dired-guess-shell-znew-switches "-KPv")
(setq dired-guess-shell-alist-user
  '(("\.pdf$" "xpdf")
    ("\.html$" "firefox")
    ("\.epub$" "ebook-viewer")
    ("\.mp3$" "mpv")))


;; Hooks
(add-hook 'dired-mode-hook
	  (defun dired-hook ()
	    (keymap-local-set "c" #'dired-create-empty-file)
            (dired-omit-mode t)
            (hl-line-mode)
	    (diredfl-mode t)))

;; Functions
(defun dired-common-dirs (key)
"Open locations which are known to you in the file system.
KEY represents a $HOME directory"
  (interactive "sr ~/remote, o ~/Org, b ~/bin, s ~/src, dw ~/Downloads, dk ~/Desktop: ")
  (cond ((equal key "r") (dired-other-window "~/remote"))
	       ((equal key "o") (dired-other-window "~/Org"))
	       ((equal key "b") (dired-other-window "~/bin"))
	       ((equal key "s") (dired-other-window "~/src"))
	       ((equal key "dw") (dired-other-window "~/Downloads"))
	       ((equal key "dk") (dired-other-window "~/Desktop"))))

;; Keybindings
(keymap-global-unset "C-x d")
(keymap-global-set "C-x l" (defun dired-site-lisp()(interactive)(dired "~/.emacs.d/lisp")))
(keymap-global-set "C-x d" 'dired-jump)
(keymap-global-set "C-x C-f" 'dired)
(keymap-global-set "C-c o" 'dired-common-dirs)

(provide 'dired-conf)
;;; dired-conf.el ends here
