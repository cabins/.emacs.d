;;; init-golang.el --- Initialize go-mode with lsp
;;; Commentary:
;;; Code:

;;; ========== Settings for Golang Development ==========
(use-package go-mode
  :mode (("\\.go'" . go-mode))
  :hook ((go-mode . lsp-deferred)
	 (go-mode . flycheck-mode))
  :commands (lsp company-lsp)
  )

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(provide 'init-golang)

;;; init-golang.el ends here
