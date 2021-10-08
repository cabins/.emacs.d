;;; init-lang.el --- settings for languages of lsp mode
;;; Commentary:
;; (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

;; GO MODE
(use-package go-mode
  :config
  (use-package go-fill-struct)
  (use-package go-impl)
  (use-package go-gen-test)
  (use-package go-tag))

;; LISP MODE
;; (use-package paredit :init (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))
(use-package lispy :init (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

;; PYTHON MODE
(defun python-isort ()
  "Sort the imports with isort."
  (interactive)
  (if (not (executable-find "isort"))
      (message "[ERROR]: <isort> not found!")
    (shell-command-on-region (point-min) (point-max) "isort --atomic --profile=black -" (current-buffer) t)))

(add-hook 'python-mode-hook (lambda ()
			      (add-hook 'before-save-hook #'python-isort nil t)))

(defun python-remove-all-unused-imports ()
  "Remove all the unused imports, do NOT use pyimport, as it has bugs.
eg.from datetime import datetime."
  (interactive)
  (if (not (executable-find "autoflake"))
      (message "[ERROR]: <autoflake> not found!")
    (shell-command (format "autoflake -i --remove-all-unused-imports %s" (buffer-file-name)))
    (revert-buffer t t t)))

;; RUST MODE
(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run))

;; VUE.js MODE
(use-package vue-mode
  ;; disable the ugly background color
  ;; [refs] https://github.com/AdamNiederer/vue-mode#how-do-i-disable-that-ugly-background-color
  :config (set-face-background 'mmm-default-submode-face nil))

;; WEB MODE
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

(provide 'init-lang)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-lang.el ends here
