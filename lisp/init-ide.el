;;; init-ide.el --- configuration for IDE programming -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: github.com/cabins

;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for LSP MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq lsp-auto-guess-root t
	lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'lsp-ui-mode-hook 'lsp-modeline-code-actions-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; If you like debugging in Emacs, enable the next lines.
;; I disabled it, as it imports too many dependencies.Such as:
;; posframe,lsp-treemacs(dash, treemacs[hydra(cl-lib), ace-window(avy)])
;; (use-package dap-mode
;;   :init
;;   (add-hook 'lsp-mode-hook 'dap-mode)
;;   (add-hook 'dap-mode-hook 'dap-ui-mode)
;;   (add-hook 'dap-mode-hook 'dap-tooltip-mode)
;;   (add-hook 'python-mode-hook (lambda() (require 'dap-python)))
;;   (add-hook 'go-mode-hook (lambda() (require 'dap-go)))
;;   (add-hook 'java-mode-hook (lambda() (require 'dap-java))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for Program Languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Golang
(use-package go-mode
  :config
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-gen-test)
  (use-package go-tag))

;; Lisp
;; You can choose paredit or lispy,but I use none of them,as they defined too many keybindings.
;; (use-package paredit :init (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))
;; (use-package lispy :init (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

;; Python
(defmacro check-run-execute (exec-file &rest body)
  "Find the EXEC-FILE and run the BODY."

  `(if (not (executable-find ,exec-file))
       (message "[ERROR]: <%s> not found!" ,exec-file)
     ,@body))

;;;###autoload
(defun python-isort ()
  "Sort the imports with isort."
  (interactive)
  (check-run-execute "isort"
		     (shell-command-on-region
		      (point-min) (point-max)
		      "isort --atomic --profile=black -"
		      (current-buffer) t)))

;;;###autoload
(defun python-remove-all-unused-imports ()
  "Remove all the unused imports, do NOT use pyimport, as it has bugs.
eg.from datetime import datetime."
  (interactive)
  (check-run-execute "autoflake"
		     (shell-command
		      (format "autoflake -i --remove-all-unused-imports %s" (buffer-file-name)))
		     (revert-buffer t t t)))

(add-hook 'python-mode-hook
	  (lambda ()
	    (add-hook 'before-save-hook #'python-isort nil t)
	    (define-key python-mode-map (kbd "C-c p s") 'python-isort)
	    (define-key python-mode-map (kbd "C-c p r") 'python-remove-all-unused-imports)))

;; Rust
(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

;; Vue.js
(use-package vue-mode
  ;; disable the ugly background color
  ;; [refs] https://github.com/AdamNiederer/vue-mode#how-do-i-disable-that-ugly-background-color
  :config (set-face-background 'mmm-default-submode-face nil))

;; Web Developemnt (html, css, js)
(use-package web-mode
  :init (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  :config (setq web-mode-enable-current-element-highlight t))
;; use C-j to expand emmet
(use-package emmet-mode
  :init
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode))

(use-package json-mode)
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package restclient
  :init (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
(use-package yaml-mode)

(provide 'init-ide)
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ide.el ends here
