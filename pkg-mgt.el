;;; pkg-mgt.el --- Package Management

;;; Commentary:
;;; Configuration of Emacs package related functionality.

;;; Code:

;;;Milkypostman’s Emacs Lisp Package Archive. Melpa. See https://melpa.org/.
(require 'package)

;; Comment/uncomment this line to enable MELPA Stable if desired.
;;See `package-archive-priorities`
;;and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar package-list '() "package list")

(package-initialize)

(provide 'pkg-mgt)
;; pkg-mgt.el ends here
