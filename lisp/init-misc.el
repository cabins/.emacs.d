;;; init-misc.el --- Summary
;;; Commentary:
;;; Code:

;; Settings for delete multi line spaces
(use-package emacs
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)
  :hook ((before-save . delete-trailing-whitespace)
	     (after-init . delete-selection-mode)))

;; Show parentheses
(use-package paren
  :config (setq show-paren-style 'parenthesis
		        show-paren-when-point-in-periphery t
		        show-paren-when-point-inside-paren nil)
  :hook (after-init . show-paren-mode))

;; Settings for electric-pair
(use-package electric
  :hook ((after-init . electric-indent-mode)
	     (prog-mode . electric-pair-mode)))

(provide 'init-misc)
;;; init-misc.el ends here
