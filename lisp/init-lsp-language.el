
;;; init-lsp-language.el --- settings for languages of lsp mode
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

;; Common Tools
(use-package reformatter)
(use-package format-all
  :hook (prog-mode . format-all-mode))

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

(use-package go-mode
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
(use-package python-mode
  :ensure nil
  :hook (python-mode . (lambda()(add-hook 'before-save-hook #'python-isort)))
  :config (use-package pyimport))

;;;###autoload
(reformatter-define python-isort
  :program "isort"
  :args '("--stdout" "--atomic" "--profile=black" "-"))

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
        web-mode-enable-current-element-highlight t)
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
