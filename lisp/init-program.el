;;; init-program.el --- Initialize Programming language with LSP mode
;;; Commentary:
;;; Code:

;;; ================================================================================
;;;                          Common LSP settings
;;; ================================================================================
(use-package lsp-mode
  :hook (prog-mode . lsp-deferred)
  :commands (lsp lsp-deferred)
  )

;;; Optionally: lsp-ui, company-lsp
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package company-lsp
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends))





;;; ================================================================================
;;;                   Settings for Programming Languages
;;; ================================================================================




;;; ------------------------------ Go ------------------------------
;; export GO111MODULE=on; go get -v golang.org/x/tools/gopls@latest
(use-package go-mode
  :mode (("\\.go'" . go-mode))
  )

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)



;;; ------------------------------ Python ------------------------------
;; pip install python-language-server[all]
;; pip install pyls-mypy
;; pip install pyls-isort
;; pip install pyls-black
(use-package python-mode
  :mode (("\\.py'" . python-mode))
  :config
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq python-indent-offset 4)
  )



(provide 'init-program)

;;; init-lsp.el ends here
