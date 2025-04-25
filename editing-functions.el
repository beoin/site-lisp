;;; editing-functions.el --- Functions related to text editing -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(defun mark-or-prompt (&optional prompt)
  "If mark is active return the region as a string otherwise PROMPT for input."
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string (or prompt "$> "))))

(defun forward-line-quarters (&optional arg)
  "Move point across the current line in quarters.
A prefix ARG between 1 and 4 moves the point forward.
The default value for ARG is 1."
  (interactive "p")
  (if (> arg 4)
      (error "Acceptable arguments between 1 and 4"))
  (or arg (setq arg 1))
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (quart (/ arg 4.0))
         (pos (+ begin (* (- end begin) quart))))
    (goto-char (round pos))))

(defun backward-line-quarters (&optional arg)
  "Move point across the current line in quarters.
A prefix ARG between 1 and 4 moves the point backward.
The default value for ARG is 1."
  (interactive "p")
  (if (> arg 4)
      (error "Acceptable arguments between 1 and 4"))
  (or arg (setq arg 1))
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (quart (/ arg 4.0))
         (pos  (- end (* (- end begin) quart))))
    (goto-char (round pos))))

;; Prevent a whitespace only string being saved to the kill ring.
(setq kill-transform-function
      (lambda (string)
        (and (not (string-blank-p string))
             string)))

(defun replace-char ()
  "Replace the char at point."
  (interactive)
  (let ((c (read-char "Replace with: ")))
    (progn
      (delete-char 1)
      (insert c)
      (backward-char))))

(defun quick-zap-up-to-char (p c)
  "The same as zap up to char, but without the mini buffer prompt.
P: The prefix argument or the count.
C: The character to zap up to."
  (interactive "P\nc")
  (let ((cnt (cond ((null p) 1)
                   ((symbolp p) -1)
                   (t p))))
    (zap-up-to-char cnt c)))

(defun delete-current-line ()
  "Delete the current line."
  (interactive)
  (delete-line))

(defun move-to-line-middle ()
  "Move point to the middle of the line."
  (interactive)
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (middle (/ (+ end begin) 2)))
    (goto-char middle)))

(defun backward-whitespace ()
  "Move the point backwards until the next available space."
	 (interactive)
	 (forward-whitespace -1))

(defun copy-word-at-point ()
  "Copy the current word under point and add it to the kill ring."
  (interactive)
  (let ((curr (point))
        (end  (prog2
               (forward-word)
               (point))))
    (copy-region-as-kill curr end)
    (goto-char curr)))

(defun copy-def-at-point ()
  "Copy the current definition at point and add it to the kill ring."
  (interactive)
  (let ((curr (point))
        (end (goto-char (1- (re-search-forward "[ \t]+\\|\n" nil 'move)))))
    (copy-region-as-kill curr end)
    (goto-char curr)))

(defun capitalise-word ()
  "Variant of builtin \"capitalize-word\". Capitalise the word at point."
  (interactive)
  (backward-whitespace)
  (capitalize-word 1))

(defun downcase-word-start ()
  "Downcase from the beginnning of the word at point.
Variant of the builtin \"downcase-word\"."
  (interactive)
  (if (beginning-of-word-p)
      (downcase-word 1)
      (backward-word)
      (downcase-word 1)))

(defun upcase-word-start ()
  "Upcase from the beginning of the word at point.
Variant of the builtin \"upcase-word\""
  (interactive)
  (if (beginning-of-word-p)
      (upcase-word 1)
    (progn
      (backward-word)
      (upcase-word 1))))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With ARG, repeat that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With ARG, repeat that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun kill-line-backward ()
  "Kill the line at point from its beginning."
  (interactive)
  (progn
    (beginning-of-line)
    (kill-line)))

(defun open-line-below-point ()
  "Create a newline below point and move to it."
  (interactive)
  (end-of-line)
  (newline))

(defun display-prefix (arg)
  "Display the value of the raw prefix ARG."
  (interactive "P")
  (if (equal current-prefix-arg '(4))
      (message "%s" (car arg))
    (message "%s" arg)))

(provide 'editing-functions)
;;; editing-functions.el ends here
