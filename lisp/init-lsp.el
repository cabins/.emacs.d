;;; init-lsp.el --- Initialize LSP mode
;;; Commentary:
;;; Code:

;;; Common LSP settings
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  )

;; Optionally
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package company-lsp
  :after lsp-mode
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(provide 'init-lsp)

;;; init-lsp.el ends here
