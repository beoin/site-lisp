;; dired-conf.el --- dired config  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; extra Dired functionality.
(require 'dired-x)

;; less commonly used parts of dired
(require 'dired-aux)
(setq dired-create-destination-dirs 'ask)

;; run a find command and dired the output
(require 'find-dired)
(setq find-name-arg "-iname")

;; rename files editing their names in dired buffers
(require 'wdired)
(setq wdired-use-interactive-rename 't)

;; directory-browsing commands
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-goa --group-directories-first")
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-ls-F-marks-symlinks t)
(setq dired-deletion-confirmer 'y-or-n-p)
(setq dired-dwim-target t)
(setq dired-clean-confirm-killing-deleted-buffers nil)
(setq dired-movement-style 'bounded)
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
	    (keymap-local-set "V" #'dired-view-file-other-window)
	    (keymap-local-set "<deletechar>" #'dired-do-delete-skip-trash)
            (dired-omit-mode t) ;; dired-x
            (hl-line-mode)
	    (diredfl-mode t)))

;; Functions
(defun dired-common-dirs (key)
"Open a Dired in a common directory (KEY)."
  (interactive
   "sr ~/remote, h ~/home, o ~/Org, b ~/bin, s ~/src, dw ~/Downloads, dk ~/Desktop:, dc ~/Documents ")
  (cond ((equal key "r") (dired "~/remote"))
        ((equal key "h") (dired "~/"))
        ((equal key "o") (dired "~/Org"))
        ((equal key "b") (dired "~/bin"))
        ((equal key "s") (dired "~/src"))
        ((equal key "dw") (dired "~/Downloads"))
        ((equal key "dk") (dired "~/Desktop"))
        ((equal key "dc") (dired "~/Documents"))
        (t (error "Error: [%s] not found" key))))

(defun find-iname-dired (pattern)
  "Find case-insensitive \"wildcard\" PATTERN in an interactivly selected directory."
  (interactive "sFind: ")
  (find-dired (read-directory-name "In Directory: ") (format "-iname '*%s*'" pattern)))

(defun dired-view-file-other-window ()
  "View file in View mode in another window from a Dired buffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (dired file))
      (view-file-other-window file))))

(defun dired-first-file ()
  "Move to the first file in a Dired buffer."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1))

(defun dired-last-file ()
  "Move to the last file in a Dired buffer."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun dired-count-files ()
  "Print the number of files in a Dired buffer."
  (interactive)
  (let* ((curr (point))
        (beg (prog2
               (dired-first-file)
               (point)))
        (end (prog2
               (dired-last-file)
               (point)))
        (lines (count-lines beg end)))
    (message "%s files" lines)
    (goto-char curr)))

(defun dired-do-delete-skip-trash (&optional arg)
  "Delete file under point and optionally the following ARG files."
  (interactive "P")
    (let ((delete-by-moving-to-trash nil))
      (dired-do-delete arg)))


;; Keybindings
(keymap-global-unset "C-x d")
(keymap-global-set "C-x l" (defun dired-site-lisp()(interactive)(dired "~/.emacs.d/lisp")))
(keymap-global-set "C-x d" 'dired-jump)
(keymap-global-set "C-c o" 'dired-common-dirs)

(provide 'dired-conf)
;;; dired-conf.el ends here
