;;; predicate-functions.el --- functions returning 't or nil -*- lexical-binding: t; -*-

;;; Commentary:
;;; predicate functions used throughout config.

;;; Code:

(defun end-of-word-p ()
  "Predicate to determine if the point is at the end of a word."
  (let ((ch (char-after (+ 1 (point)))))
    (cond ((equal 32 ch) t)
          ((equal 10 ch) t)
          ((equal 46 ch) t)
          (t nil))))

(defun beginning-of-word-p ()
  "Predicate to determine if the point is at the beginning of a word."
  (let ((ch (preceding-char)))
    (cond ((equal 32 ch) t)
          ((equal 10 ch) t)
          ((equal 34 ch) t)
          (t nil))))

(defun current-line-empty-p ()
  "Return non-nil if point is on an empty line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(provide 'predicate-functions)
;;; predicate-functions.el ends here
