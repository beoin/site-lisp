;;; early-init.el --- Emacs startup config -*- no-byte-compile: t -*-

;;; Commentary:
;;; Customizations that take effect during Emacs startup.
;;; Code:

;; alloc.c storage allocation and gc for lisp interpreter
(setq gc-cons-threshold most-positive-fixnum)
(setq garbage-collection-messages 't)

;; lread.c lisp parsing and input streams
(setq load-prefer-newer 't)

;; commands for multilingual environment
;;(require 'mule-cmds) not provided
(set-language-environment "UTF-8")
(setq default-input-method nil)
(set-default-coding-systems 'utf-8)

;; font.c font primitives.
(setq inhibit-compacting-font-caches 't)

;; eval.c evaluator for lisp interpreter
(setq lisp-eval-depth-reserve 300)

;; frame.c generic frame functions
(setq frame-inhibit-implied-resize 't)

;; define a default menu bar
(require 'menu-bar)
(menu-bar-mode -1)

;; window system-independent scroll bar support
(require 'scroll-bar)
(scroll-bar-mode -1)

;; setting up the tool bar
(require 'tool-bar)
(tool-bar-mode -1)

;; process shell arguments
;;(require 'startup) not provided
(setq inhibit-startup-screen 't)
(setq inhibit-splash-screen 't)

(provide 'early-init)
;;; early-init.el ends here
