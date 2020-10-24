;;; init-python.el --- Python settings

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

(use-package python-mode
  :init (setq-default python-indent-offset 4
                      python-indent-guess-indent-offset-verbose nil))

(use-package py-isort
  :init (setq python-sort-imports-on-save t))

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :after lsp-mode python-mode
  :hook (python-mode . (lambda()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))

(provide 'init-python)
;;; init-python.el ends here
