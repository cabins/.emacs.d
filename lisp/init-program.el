;;; init-program.el --- Initialize Programming language with LSP mode

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

(use-package go-mode)

(use-package markdown-mode)

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package yaml-mode)

(use-package lsp-mode
  ;; add prog-mode to lsp instead of adding one by one
  ;; :hook (prog-mode . (lsp-deferred))
  :hook ((python-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (html-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init (setq lsp-prefer-flymake nil ;; Prefer not use flymake(instead flycheck)
              lsp-keep-workspace-alive nil ;; Auto kill LSP server
              lsp-enable-indentation nil
              lsp-enable-on-type-formatting nil
              lsp-auto-guess-root nil
              lsp-enable-snippet t)
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
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-use-webkit nil
              lsp-ui-doc-delay 0.2
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'at-point
              lsp-eldoc-enable-hover nil ;; Disable eldoc displays in minibuffer
              lsp-ui-sideline-enable t
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-ignore-duplicate t)
  :config (setq lsp-ui-flycheck-enable t)
  :commands lsp-ui-mode)

(use-package company-lsp
  :after company lsp-mode
  :init (push 'company-lsp company-backends))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; (use-package dap-mode
;;   :diminish
;;   :hook ((after-init . dap-mode)
;;          (dap-mode . dap-ui-mode)
;;          (python-mode . (lambda() (require 'dap-python)))
;;          (go-mode . (lambda() (require 'dap-go)))
;;          (java-mode . (lambda() (require 'dap-java)))))

(use-package web-mode
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-css-colorization t)
  (use-package company-web
    :config
    (add-to-list 'company-backends 'company-web-html)
    (add-to-list 'company-backends 'company-css))
  )

(use-package emmet-mode
  :hook (web-mode css-mode))

(use-package emacs
  :hook (prog-mode . electric-pair-mode))

(provide 'init-program)
;;; init-program.el ends here
