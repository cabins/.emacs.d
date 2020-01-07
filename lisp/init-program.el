;;; init-program.el --- Initialize Programming language with LSP mode
;;; Commentary:
;;; Code:

;;; ================================================================================
;;;                          Common LSP settings
;;; ================================================================================
(use-package lsp-mode
  :hook (prog-mode . lsp)
  :commands (lsp lsp-deferred)
  )

;;; Optionally: lsp-ui, company-lsp
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t)
  :commands lsp-ui-mode)

(use-package company-lsp
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode)



;;; ================================================================================
;;;                   Settings for Programming Languages
;;; ================================================================================

(eldoc-mode -1)
(global-eldoc-mode -1)
(setq lsp-pyls-server-command "python3.8")

;;; ------------------------------ Go ------------------------------
;; export GO111MODULE=on; go get -v golang.org/x/tools/gopls@latest
(use-package go-mode
  :mode (("\\.go'" . go-mode))
  :hook (go-mode . lsp)
  :config
  (setq indent-tabs-mode t)
  (setq tab-width 4)
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
  (setq python-shell-interpreter "python3.8")
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq python-indent-offset 4)
  (setq lsp-clients-python-command "/opt/python38/bin/pyls")
  (add-hook 'python-mode-hook
	    '(lambda () (eldoc-mode -1)) t)
  )



(provide 'init-program)

;;; init-lsp.el ends here
