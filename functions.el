;;; functions.el --- Utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;;; Convenience functions common across configs

;;; Code:

(defun elisp-info (node-name)
  "Search the builtin Emacs Lisp information pages.
Takes a String NODE-NAME, used search to documentation tree."
  (interactive "sNode: ")
  (info (format "(elisp)%s" node-name)))

(defun search-github-ext (term ext)
  "Serach Github with a TERM and extension EXT."
  (interactive "sGithub Term: \nsExt: ")
  (browse-url (format "https://github.com/search?q=%s+path:*.%s" term ext)))
  
(defun search-query (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them."
  `(defun ,(intern (format "search-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (search-query ,search-engine-url ,search-engine-prompt)))

(install-search-engine "github" "https://github.com/search?q= " "Github: ")
(install-search-engine "wikipedia" "https://en.wikipedia.org/w/index.php?search= " "Wikipedia: ")
(install-search-engine "define" "https://www.google.com/search?hl=en&q=define " "Define: ")
(install-search-engine "duckduckgo" "https://duckduckgo.com/?q=" "DuckDuckGo: ")
(install-search-engine "google"  "https://www.google.com/search?hl=en&q=" "Google: ")
(install-search-engine "youtube" "https://www.youtube.com/results?search_query=" "Youtube: ")
(install-search-engine "book" "https://www.amazon.com/s?k=" "Book: ")
(install-search-engine "archwiki" "https://wiki.archlinux.org/index.php?search=" "Arch Wiki: ")

(defun dired-common-dirs  (key)
  "A function in progesss. Open locations which are known to you in the file system.
KEY represents a $HOME directory"
  (interactive "sr ~/remote, o ~/Org, b ~/bin, s ~/src, dw ~/Downloads, dk ~/Desktop: ")
  (cond ((equal key "r") (dired-other-window "~/remote"))
	       ((equal key "o") (dired-other-window "~/Org"))
	       ((equal key "b") (dired-other-window "~/bin"))
	       ((equal key "s") (dired-other-window "~/src"))
	       ((equal key "dw") (dired-other-window "~/Downloads"))
	       ((equal key "dk") (dired-other-window "~/Desktop"))
	       ))

;;https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

(defun status ()
  (interactive)
  (eshell-command "status"))

(provide 'functions)
;;; functions.el ends here
