;;; functions.el --- Utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;;; functions not related to text editing.

;;; Code:

(defun messages-buffer ()
  "Switch to the *Messages* buffer.
Prefer instead the builtin function \"view-echo-area-messages\"."
  (interactive)
  (pop-to-buffer-same-window "*Messages*"))

(defun show-file-name ()
  "Echo the full path of the current file."
  (interactive)
  (message (buffer-file-name)))

(defun elisp-info (node-name)
  "Search the builtin Emacs Lisp information pages.
Takes a String NODE-NAME, used search to documentation tree."
  (interactive "sNode: ")
  (info (format "(elisp)%s" node-name)))

(defun search-github-ext (term ext)
  "Serach Github with a TERM and extension EXT."
  (interactive "sGithub Term: \nsExt: ")
  (browse-url (format "https://github.com/search?q=%s+path:*.%s" term ext)))

(defun search-google-books (book)
  "Serach for a BOOK with Google Books."
  (interactive "sBook: ")
  (browse-url
   (format "https://www.google.com/search?udm=36&q=%s" (string-replace " " "+" book))))

(defun google-define (word)
  "Get a dictionary definition of WORD from Google search."
  (interactive "sDefine: ")
  (browse-url
   (format "https://www.google.com/search?hl=en&q=define %s" word)))
  
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
  "Call the status shell file."
  (interactive)
  (shell-command "status"))

(defun derived-modes (mode)
  "Return a list of the ancestor modes that MODE is derived from.
MODE must be a symbol"
  (let ((modes   ())
        (parent  nil))
    (while (setq parent (get mode 'derived-mode-parent))
      (push parent modes)
      (setq mode parent))
    (setq modes  (nreverse modes))))

(defun delete-frame-or-quit ()
  "Delete the selected frame & kill terminal buffers. If the last frame, kill Emacs."
  (interactive)
  (kill-matching-buffers "*shell" nil t)
  (when (condition-case nil (delete-frame)
          (error (save-buffers-kill-emacs))))
  (select-frame-set-input-focus (selected-frame)))

(defun toggle-window-split ()
  "Move from a horizontal to a vertical split and vice versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (message "Exactly 2 windows required.")))

(defun read-char-as-string (&optional PROMPT)
  "Read a char from minibuffer as a string value."
  (let ((key (read-key PROMPT)))
    (char-to-string (if (characterp key) key 27))))

(defun lookup-word-at-point (word)
  "Lookup the WORD at point."
  (interactive (list (current-word)))
  (let ((url "https://www.google.com/search?hl=en&q=define+"))
    (browse-url (concat url word))))

(provide 'functions)
;;; functions.el ends here
