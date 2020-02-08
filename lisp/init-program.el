;;; init-program.el --- Initialize Programming language with LSP mode
;;; Commentary:
;;; Code:

;; (use-package lsp-java :defer t)
(use-package go-mode :defer t)
(use-package markdown-mode :defer t)
(use-package restclient
  :defer t
  :mode ("\\.http\\'" . restclient-mode))
(use-package yaml-mode :defer t)

;;; ================================================================================
;;;                          Common LSP settings                                 ;;;
;;; ================================================================================

(use-package lsp-mode
  ;; add prog-mode to lsp instead of adding one by one
  ;; :hook (prog-mode . (lsp-deferred))
  :hook ((python-mode . lsp)
         (go-mode . lsp)
         (js-mode . lsp)
         (web-mode . lsp)
         (html-mode . lsp))
  :commands (lsp lsp-deferred)
  :init (setq lsp-prefer-flymake nil
              lsp-auto-guess-root t)
  :config
  ;; Configure LSP Clients
  (use-package lsp-clients
    :ensure nil
    :functions (lsp-format-buffer lsp-organize-imports)
    :hook (go-mode . (lambda ()
                       "Format buffer and auto-import packages"
                       (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t)))))

;;; Optionally: lsp-ui, company-lsp
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-flycheck-enable t)
  :commands lsp-ui-mode)

(use-package company-lsp
  :after company lsp-mode
  :init (push 'company-lsp company-backends))

(use-package lsp-treemacs
  :defer t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :diminish
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (python-mode . (lambda() (require 'dap-python)))
         (go-mode . (lambda() (require 'dap-go)))
         (java-mode . (lambda() (require 'dap-java)))))

(provide 'init-program)
;;; init-program.el ends here
