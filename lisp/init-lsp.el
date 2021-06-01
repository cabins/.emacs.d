;;; init-lsp --- lsp settings

;;; Commentary:
;;; (c)Cabins, github.com/cabins/.emacs.d

;;; Code:

(use-package lsp-mode
  ;; add prog-mode to lsp instead of adding one by one
  ;; :hook ((prog-mode . (lsp-deferred))
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (prog-mode . (lambda() (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)(lsp-deferred))))
         )
  :init (setq lsp-keep-workspace-alive nil ;; Auto kill LSP server
              lsp-enable-indentation t
              lsp-enable-on-type-formatting t
              lsp-auto-guess-root t
              lsp-enable-snippet t
              lsp-prefer-flymake t
              lsp-prefer-capf t
              lsp-modeline-diagnostics-enable t
              lsp-modeline-diagnostics-scope :workspace ;; workspace/global/file
              lsp-idle-delay 0.500
              read-process-output-max (* 1024 1024) ;; 1MB
              lsp-completion-provider :capf)
  :config
  ;; Configure LSP Clients
  (use-package lsp-clients
    :ensure nil
    :functions (lsp-format-buffer lsp-organize-imports)))

;;; Optionally: lsp-ui, company-lsp
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-ui-mode . lsp-modeline-code-actions-mode)
         ;; (lsp-ui-mode . lsp-ui-peek-mode) ;; drop it 'cause it has BUGs
         )
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-use-webkit nil
              lsp-ui-doc-delay .3
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'at-point ;; top/bottom/at-point
              lsp-eldoc-enable-hover t ;; eldoc displays in minibuffer
              lsp-ui-sideline-enable nil
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-code-actions t
              lsp-ui-sideline-show-diagnostics t
              lsp-ui-sideline-ignore-duplicate t
              lsp-modeline-code-actions-segments '(count name)
              lsp-headerline-breadcrumb-enable nil)
  :config
  (setq lsp-ui-flycheck-enable nil)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (when (display-graphic-p)
    (treemacs-resize-icons 14))
  )


(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :init
  (when (display-graphic-p)
    (treemacs-resize-icons 14)))

(use-package dap-mode
  :diminish
  :hook ((lsp-mode . dap-mode)
         (dap-mode . dap-ui-mode)
	     (dap-mode . dap-tooltip-mode)
         (python-mode . (lambda() (require 'dap-python)))
         (go-mode . (lambda() (require 'dap-go)))
         (java-mode . (lambda() (require 'dap-java)))))

(provide 'init-lsp)
;;; init-lsp.el ends here
