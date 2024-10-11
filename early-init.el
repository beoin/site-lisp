;;; early-init.el --- early bird -*- no-byte-compile: t -*-

;;; Commentary:
;;; Code:
(setq gc-cons-threshold most-positive-fixnum)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-spash-screen t)
(customize-set-variable 'load-prefer-newer t)

(provide 'early-init)
;;; early-init.el ends here
