;;; init-golang.el --- Initialize go-mode with lsp
;;; Commentary:
;;; Code:

(use-package go-mode
  :defer t
  :mode (("\\.go'" . go-mode))
  :hook (progn
	  (go-mode . lsp)
	  (go-mode . yas-minor-mode)
	  )
  )

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(provide 'init-golang)

;;; init-golang.el ends here
