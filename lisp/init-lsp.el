;;; init-lsp --- lsp settings

;;; Commentary: (c)Cabins, github.com/cabins/.emacs.d
;;; lsp-mode is the Emacs client for LSP server.
;;; Once you install the lsp-mode and the language server,
;;; you can call `M-x lsp' to autostart the server

;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-format-buffer lsp-organize-imports)
  :init
  (add-hook 'lsp-mode-hook (lambda ()
			     (lsp-enable-which-key-integration)
			     (add-hook 'before-save-hook #'lsp-organize-imports t t)
			     (add-hook 'before-save-hook #'lsp-format-buffer t t)))
  (add-hook 'prog-mode-hook (lambda()
			      (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)(lsp-deferred))))
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t nil)))
  :init
  (setq lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-border "dim gray"
        lsp-ui-sideline-ignore-duplicate t
        lsp-headerline-breadcrumb-enable nil)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-ui-mode-hook 'lsp-modeline-code-actions-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package dap-mode
  :init
  (add-hook 'lsp-mode-hook 'dap-mode)
  (add-hook 'dap-mode-hook 'dap-ui-mode)
  (add-hook 'dap-mode-hook 'dap-tooltip-mode)
  (add-hook 'python-mode-hook (lambda() (require 'dap-python)))
  (add-hook 'go-mode-hook (lambda() (require 'dap-go)))
  (add-hook 'java-mode-hook (lambda() (require 'dap-java))))

(provide 'init-lsp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lsp.el ends here
