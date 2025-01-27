;; dired-conf.el --- dired config  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Vars
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-sh --group-directories-first")
(defvar dired-recursive-deletes 'always)
(defvar dired-recursive-copies 'always)
(defvar dired-create-destination-dirs 'ask)
(defvar dired-ls-F-marks-symlinks t)
(defvar dired-deletion-confirmer 'y-or-n-p)
(defvar dired-dwim-target t)
(defvar dired-clean-confirm-killing-deleted-buffers nil)
(defvar dired-guess-shell-znew-switches "-KPv")
(defvar dired-guess-shell-alist-user
  '(("\.pdf$" "xpdf")
    ("\.html$" "firefox")
    ("\.mp3$\\|.mp4$\\|.mkv$\\|" "mpv")
    ("\.epub$\\|.mobi$\\|.azw3$|" "ebook-viewer")))


;; Hooks
(add-hook 'dired-mode-hook
	  (defun dired-hook ()
	    (keymap-local-set "c" #'dired-create-empty-file)
            (dired-omit-mode t)
            (hl-line-mode)
	    (diredfl-mode t)))

;; Functions
(defun dired-common-dirs (key)
"A function in progesss. Open locations which are known to you in the file system.
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
