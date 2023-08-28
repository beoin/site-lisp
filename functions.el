;;; functions.el --- Utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;;; Convenience functions common across configs

;;; Code:
(defun lisp-hook ()
  (paredit-mode)
  (rainbow-blocks-mode)
  (rainbow-delimiters-mode)
  (prettify-symbols-mode))

(defun elisp-info (node-name)
  "Search the builtin Emacs Lisp information pages.
Takes a String NODE-NAME, used search to documentation tree."
  (interactive "sNode: ")
  (info (format "(elisp)%s" node-name)))

(defun firefox ()
  "Launch Firefox"
  (interactive)
  (browse-url "about:profiles"))

(defun firefox-with-profile (p)
  "Launch Firefox with a named configuration PROFILE."
  (interactive "sGoogle (g), WhatsApp (w) Reddit (r): ")
  (let ((profile (cond ((equal p "g") "Google")
			  ((equal p "w") "WhatsApp")
			  ((equal p "r") "Reddit"))))
	   (start-process-shell-command "firefox" nil (format "firefox -P  %s" profile))))

(defun firefox-with-cookies (url)
  "Launch Firefox in Cookies profile with named URL."
  (interactive "s Firefox with Cookies URL: ")
  (start-process-shell-command "firefox" nil (format "firefox -P Cookies %s" url)))

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
  "Given some information regarding a search engine, install the interactive command to search through them"
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
(install-search-engine "book" "https://www.amazon.com/s?rh=n%3A283155&dc&k=" "Book: ")
(install-search-engine "archwiki" "https://wiki.archlinux.org/index.php?search=" "Arch Wiki: ")

(defun open-known  (key file)
  "A function in progesss. Open locations which are known to you in the file system.
KEY represents a $HOME directory"
  (interactive "sr ~/remote, o ~/Org, b ~/bin, s ~/src: \nsFile: ")
  (cond ((equal key "r") (dired (concat "~/remote/" file)))
	       ((equal key "o") (find-file (concat "~/Org/" file ".org")))
	       ((equal key "b") (find-file (concat "~/bin/" file)))
	       ((equal key "s") (dired (concat "~/src/" file)))
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

(defun zig-install ()
  "Place zig executable on the system path via a symbolic link."
  (interactive)
  (letrec ((path (string-trim-right (file-name-directory (buffer-file-name)) "/src/" ) )
	   (file-name (car (last (split-string path "/"))))
	   (cmd (format "ln -sf %s/zig-out/bin/%s ~/.local/bin/" path file-name)))
    (start-process-shell-command "file-path" nil cmd)
    (message "%s installed" file-name)))

(defun zig-make-project (b-or-l name)
  "Create a new zig project."
  (interactive "s init-exe (b) or int-lib (l) : \nsProject Name: ")
  (letrec 
      ((ex (cond ((equal b-or-l "b") "init-exe")
		 ((equal b-or-l "l") "init-lib")))
       (path (format "~/src/zig/%s" name))
       (cmd  (format "zig %s" ex)))
    (mkdir path)
    (cd path)
    (copy-file "~/Resources/dumbjump" (format "%s/.dumpjump" path))
    (start-process-shell-command "zig init" nil cmd)
    (message "%s %s project created" ex name)))

(defun status ()
  (interactive)
  (eshell-command "status"))
(minibuffer-with-setup-hook 'status)


(provide 'functions)
;;; functions.el ends here

