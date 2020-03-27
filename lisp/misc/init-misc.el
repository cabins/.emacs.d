;;; init-misc.el --- Summary

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

;; Settings for delete multi line spaces
(use-package emacs
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Display 'lambda' as 'λ' (just for fun)
  (global-prettify-symbols-mode 1)
  :hook ((before-save . delete-trailing-whitespace)
	     (after-init . delete-selection-mode)))

;; Settings for the TAB behavior
(use-package emacs
  :init (setq-default tab-width 4
                      indent-tabs-mode nil))

;; Display time at the right bottom corner
(use-package emacs
  :init
  (setq display-time-24hr-format t
	    display-time-day-and-date t)
  (display-time-mode 1))


;; Settings for line number
;; Drop the global-display-line-numbers-mode on Windows platform, 'cause it make the window splash on Windows
(use-package emacs
  :unless *is-windows*
  :init
  (setq display-line-numbers-type 'relative) ;relative, visual
  (global-display-line-numbers-mode t))


;; Settings for electric-pair
(use-package electric
  :hook ((after-init . electric-indent-mode)
	     (prog-mode . electric-pair-mode)))

(provide 'init-misc)
;;; init-misc.el ends here
