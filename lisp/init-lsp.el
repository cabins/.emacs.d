;;; init-lsp.el --- Initialize LSP mode
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp))

;; Optionally
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package company-lsp
  :after lsp-mode
  :commands company-lsp)

(provide 'init-lsp)
;;; init-lsp.el ends here
