;;; early-init.el --- Emacs startup config -*- no-byte-compile: t -*-

;;; Commentary:
;;; Customizations that take effect during Emacs startup.
;;; Code:

;; Number of bytes of consing between garbage collections.
(setq gc-cons-threshold most-positive-fixnum)
;; Restore to normal value after startup (e.g. 50MB)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

(setq garbage-collection-messages t)
(setq load-prefer-newer t)
(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq inhibit-compacting-font-caches t)
(setq lisp-eval-depth-reserve 300)
(setq frame-inhibit-implied-resize 't)

;; UI
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-spash-screen t)

(provide 'early-init)
;;; early-init.el ends here
