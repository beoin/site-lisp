;; dired-conf.el --- dired config  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Vars
(lisp-extra-font-lock-global-mode 1)
(put 'dired-find-alternate-file 'disabled nil)
(defvar dired-recursive-deletes 'always)
(defvar dired-recursive-copies 'always)
(defvar dired-create-destination-dirs 'ask)
(defvar dired-ls-F-marks-symlinks t)
(defvar dired-deletion-confirmer 'y-or-n-p)
(defvar dired-dwim-target t)
(defvar dired-clean-confirm-killing-deleted-buffers nil)
(setq dired-listing-switches "-al --group-directories-first" )
(defvar dired-guess-shell-alist-user
  '(("\.pdf$" "xpdf")
    ("\.html$" "firefox")
    ("\.mp3$\\|.mp4$\\|.mkv$\\|" "mpv")
    ("\.epub$\\|.mobi$\\|.azw3$|" "ebook-viewer")))

;; Hooks
(add-hook 'dired-mode-hook
	  (defun dired-hook ()
	    (keymap-local-set "c" #'dired-create-empty-file)
	    (display-line-numbers-mode t)
	    (dired-omit-mode t)
	    (diredfl-global-mode t)))

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
(keymap-global-set "C-x l" (defun dired-site-lisp()(interactive)(dired "~/.emacs.d/lisp")))
(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x C-f") 'dired)
(keymap-global-set "C-c o" 'dired-common-dirs)

(provide 'dired-conf)
;;; dired-conf.el ends here
