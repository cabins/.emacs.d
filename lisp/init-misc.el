;;; init-misc.el --- Summary
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

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
(setq display-line-numbers-type 't) ; relative, visual
(global-display-line-numbers-mode t)

(provide 'init-misc)
;;; init-misc.el ends here
