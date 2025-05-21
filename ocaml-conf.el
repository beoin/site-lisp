;;; ocaml-conf.el --- OCaml configuration -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(require 'dune)
(require 'dune-flymake)
(require 'dune-watch)
(require 'ocamlformat)
(require 'ocp-indent)
(require 'utop)
(require 'tuareg)

;; Hooks
(add-hook 'tuareg-load-hook
          (defun tuareg-hook ()
            (auto-revert-mode)
            (electric-pair-mode)))

(provide 'ocaml-conf)
;;; ocaml-conf.el ends here
