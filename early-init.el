;;; early-init.el --- early bird -*- no-byte-compile: t -*-

;;; Commentary:
;;; Code:
(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer t)
(set-language-environment "UTF-8")
(setq default-input-method nil)
(setq inhibit-compacting-font-caches t)

;; UI
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-spash-screen t)

(provide 'early-init)
;;; early-init.el ends here
