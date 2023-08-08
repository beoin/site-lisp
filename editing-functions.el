(defun replace-char ()
  "Replace the char at point"
  (interactive)
  (setq c (read-key))
  (progn
    (delete-char 1)
    (insert c)
    (backward-char)))

;;https://gist.github.com/VernonGrant/1341a3bdcded3fc3a3741427f706ca85
;; Zap up to char quickly.
(defun vg-quick-zap-up-to-char (p c)
  "The same as zap up to char, but without the mini buffer prompt.
P: The prefix argument or the count.
C: The character to zap up to."
  (interactive "P\nc")
  (let ((cnt (cond ((null p) 1)
                   ((symbolp p) -1)
                   (t p))))
    (zap-up-to-char cnt c)))
;;(define-key global-map (kbd "C-z") 'vg-quick-zap-up-to-char)

(defun delete-current-line ()
  "Delete the current line."
  (interactive)
  (delete-line))

(defun move-to-line-middle ()
  "Move point to the middle of the line"
  (interactive)
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (middle (/ (+ end begin) 2)))
    (goto-char middle)))

(defun backward-whitespace ()
  "Move the point backwards until the next available space."
	 (interactive)
	 (forward-whitespace -1))

(defun capitalise-word ()
  "Variant of builtin capitalize-word. Capitalise the word at point."
  (interactive)
  (backward-whitespace)
  (capitalize-word 1))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun newline-above-below-point ()
  "Create a newline below point and move to it.
If \\[universal-argument] is supplied newline is created above point."
  (interactive)
  (if current-prefix-arg
      (progn
	(beginning-of-line)
	(newline)
	(previous-line))
    (progn
      (end-of-line)
      (newline))))

(provide 'editing-functions)
