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

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)
(global-set-key (kbd "C-c f") 'recentf-open-files)

;; Toggle hideshow minor mode on
(add-hook 'prog-mode-hook 'hs-minor-mode)

(provide 'init-misc)
;;; init-misc.el ends here
