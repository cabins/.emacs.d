;;; init-program.el --- Initialize Programming language with LSP mode

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp/lang" user-emacs-directory))

(require 'init-lsp)
(require 'init-go)
(require 'init-markdown)
(require 'init-restclient)
(require 'init-yaml)
(require 'init-web)

(use-package emacs
  :hook (prog-mode . electric-pair-mode))

(provide 'init-program)
;;; init-program.el ends here
