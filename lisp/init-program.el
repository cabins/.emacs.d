;;; init-program.el --- Initialize Programming language with LSP mode
;;; Commentary:
;;; Code:

;;; Basic Programming Style
(setq indent-tabs-mode t)
(setq tab-width 4)

;;; ================================================================================
;;;                          Common LSP settings
;;; ================================================================================
(use-package lsp-mode
  ;; add prog-mode to lsp instead of adding one by one
  :hook (prog-mode . lsp)
  :commands (lsp lsp-deferred)
  :config (setq lsp-prefer-flymake nil)
  )

;;; Optionally: lsp-ui, company-lsp
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-flycheck-enable t)
  :commands lsp-ui-mode
  )

(use-package company-lsp
  :after company lsp-mode
  :init (push 'company-lsp company-backends)
  )

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode)

;;; ------------------------------ Go ------------------------------
;; export GO111MODULE=on; go get -v golang.org/x/tools/gopls@latest
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook (go-mode . lsp-go-install-save-hooks))

;;; ----------------------------- Java -----------------------------
(use-package lsp-java)

(provide 'init-program)
;;; init-program.el ends here
