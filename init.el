(add-to-list 'load-path  "~/.emacs.d/lisp")
(add-to-list 'load-path  "~/remote/hare-mode")

(require 'pkg-mgt)
(require 'vars)
(require 'functions)
(require 'editing-functions)
(require 'hooks)
(require 'keybindings)
(require 'hare-mode)

(load custom-file)

(find-file-noselect "~/.bashrc")
(find-file-noselect "~/.config/sway/config")
(find-file-noselect "~/.config/river/init")
(find-file-noselect "~/Org/dhamma/suttas.org")
(find-file-noselect "~/src/rust/scratch/src/main.rs")
(find-file-noselect "~/Org/rust/rust.org")

(find-file "~/Org/todo.org")
