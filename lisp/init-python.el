;;; init-python.el --- Initialize go-mode with lsp
;;; Commentary:

;;; ============ pip dependencies ============
;; pip install python-language-server[all]
;; pip install pyls-mypy
;; pip install pyls-isort
;; pip install pyls-black
;;; ============ end ============

;;; Code:

;;; ========== Settings for Python Development ==========
(use-package python-mode
  :mode (("\\.py'" . python-mode))
  :hook ((python-mode . lsp-deferred)
	 (python-mode . flycheck-mode))
  :config
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq python-indent-offset 4)
  :commands (lsp company-lsp)
  )

(provide 'init-python)

;;; init-python.el ends here
