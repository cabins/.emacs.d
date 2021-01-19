;;; init-program.el --- Initialize Programming language with LSP mode
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

(require 'init-go)
(require 'init-json)
;; (require 'init-lisp) ; lispy keybinding tastes bad for me, so I disable it.
(require 'init-markdown)
(require 'init-python)
(require 'init-restclient)
(require 'init-rust)
(require 'init-vue)
(require 'init-web)
(require 'init-yaml)

(require 'init-lsp)

(provide 'init-program)
;;; init-program.el ends here
