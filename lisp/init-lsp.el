;;; init-lsp --- lsp settings

;;; Commentary: (c)Cabins, github.com/cabins/.emacs.d
;;; lsp-mode is the Emacs client for LSP server.
;;; Once you install the lsp-mode and the language server,
;;; you can call `M-x lsp' to autostart the server

;;; Code:
(defun lsp-lang-hooks ()
  "Add lsp hooks before save."
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (add-hook 'before-save-hook #'lsp-format-buffer t t))
  
(use-package lsp-mode
  :commands (lsp lsp-deferred lsp-format-buffer lsp-organize-imports)
  :init (setq read-process-output-max (* 1024 1024) ; 1MB, data size read from server, default on 4K
	      lsp-auto-guess-root t)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-lang-hooks)
	 (prog-mode . (lambda()(unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)(lsp-deferred))))))
  
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-ui-mode . lsp-modeline-code-actions-mode))
  :init (setq lsp-ui-doc-include-signature t
              lsp-ui-sideline-ignore-duplicate t
              lsp-modeline-code-actions-segments '(count name)
              lsp-headerline-breadcrumb-enable nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package dap-mode
  :hook ((lsp-mode . dap-mode)
         (dap-mode . dap-ui-mode)
	 (dap-mode . dap-tooltip-mode)
         (python-mode . (lambda() (require 'dap-python)))
         (go-mode . (lambda() (require 'dap-go)))
         (java-mode . (lambda() (require 'dap-java)))))

(provide 'init-lsp)


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lsp.el ends here
