;;; early-init.el --- early bird -*- no-byte-compile: t -*-

(setq gc-cons-threshold (* 50 1000 1000))

(customize-set-variable 'load-prefer-newer noninteractive)
		
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-spash-screen t)
(customize-set-variable 'load-prefer-newer t)

;;; early-init.el ends here
