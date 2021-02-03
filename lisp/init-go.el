;;; init-go.el --- Golang settings
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

(defvar go--tools '("golang.org/x/tools/cmd/goimports"
                   "github.com/go-delve/delve/cmd/dlv"
                   "github.com/josharian/impl"
                   "github.com/cweill/gotests/..."
                   "github.com/fatih/gomodifytags"
                   "github.com/davidrjenni/reftools/cmd/fillstruct")
  "Go tools may needed")

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook (go-mode . lsp-go-install-save-hooks)
  :config
  ;; go-fill-struct
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-gen-test)
  (use-package go-tag))

(provide 'init-go)
;;; init-go.el ends here
