;;; init-lsp-language.el --- settings for languages of lsp mode
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

;;; the following settings sorted by the package name (alphabet order)

;; GO MODE
;; ==============================
(defvar go--tools '("golang.org/x/tools/cmd/goimports"
                   "github.com/go-delve/delve/cmd/dlv"
                   "github.com/josharian/impl"
                   "github.com/cweill/gotests/..."
                   "github.com/fatih/gomodifytags"
                   "github.com/davidrjenni/reftools/cmd/fillstruct")
  "Go tools may needed.")

(defun lsp-go-install-save-hooks ()
  "Add hooks before save."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :hook (go-mode . lsp-go-install-save-hooks)
  :config
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-gen-test)
  (use-package go-tag))

;; JSON MODE
;; ==============================
(use-package json-mode)

;; LISP MODE
;; ==============================
(use-package paredit
  :hook (emacs-lisp-mode . enable-paredit-mode))

;; MARKDOWN MODE
;; ==============================
(use-package markdown-mode)

;; PYTHON MODE
;; ==============================
(defvar python--tools '("python-language-server[all]"
                        "black"
                        "isort")
  "Modules for Python development.")

(use-package python-mode
  :ensure nil
  :config
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset-verbose nil)
  ;; Use black to format the Python code
  (use-package blacken
    :hook ((python-mode . blacken-mode)))
  ;; Sort the pytho imports
  (use-package py-isort
    :init (setq python-sort-imports-on-save t)))


;; RESTCLIENT - HTTP CLIENT
;; ==============================
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; RUST MODE
;; ==============================
(use-package rust-mode
  :bind (:map rust-mode-map ("C-c C-c" . rust-run)) ;bind the rust-run key
  :config (setq indent-tabs-mode nil                ;rust use spaces instead of tab
                rust-format-on-save t))

;; VUE.js MODE
;; ==============================
(use-package vue-mode
  ;; disable the ugly background color
  ;; https://github.com/AdamNiederer/vue-mode#how-do-i-disable-that-ugly-background-color
  :config (set-face-background 'mmm-default-submode-face nil))

;; WEB MODE
;; ==============================
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
  (use-package emmet-mode
    :hook (web-mode css-mode)))

;; YAML MODE
;; ==============================
(use-package yaml-mode)


(provide 'init-lsp-language)
;;; init-lsp-language.el ends here
