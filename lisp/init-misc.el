;;; init-misc.el --- Summary

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

;; Settings for delete multi line spaces
(use-package emacs
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Settings for the TAB behavior
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  ;; Settings for line number
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)

  ;; Display time at the right bottom corner
  (display-time-mode 1)
  :hook ((before-save . delete-trailing-whitespace)
	     (after-init . delete-selection-mode)))

;; Settings for electric-pair
(use-package electric
  :hook ((after-init . electric-indent-mode)
	     (prog-mode . electric-pair-mode)))

(provide 'init-misc)
;;; init-misc.el ends here
