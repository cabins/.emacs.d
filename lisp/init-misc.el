;;; init-misc.el --- Summary

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

;; alias yes/no to y/p
(defalias 'yes-or-no-p 'y-or-n-p)

;; display 'lambda' as 'λ' (just for fun)
(global-prettify-symbols-mode 1)

;; <TAB> show settings
(setq-default tab-width 4
              indent-tabs-mode nil)

;; C-s/C-r settings
(setq-default isearch-lazy-count t
              lazy-count-prefix-format "%s/%s ")

;; some delete hooks settings
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; settings for line number
;; drop the global-display-line-numbers-mode on Windows platform,
;; 'cause it make the window splash on Windows
(unless *is-windows*
  (setq display-line-numbers-type 'visual) ; relative, visual
  (global-display-line-numbers-mode t))



(provide 'init-misc)
;;; init-misc.el ends here
