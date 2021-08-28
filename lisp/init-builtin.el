;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code: Sorted by Alphbet order

;; Abbrev
(setq-default abbrev-mode t)

;; Cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)

;; Delete Behavior
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Electric-Pair
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Flymake
(add-hook 'prog-mode-hook 'flymake-mode)

;; HideShow Minor Mode
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; Ido ( instead of ivy & counsel & swiper)
(setq-default ido-auto-merge-work-directories-length -1
	      ido-enable-flex-matching t
	      ido-everywhere t
	      ido-use-filename-at-point t
	      isearch-lazy-count t
	      lazy-count-prefix-format "%s/%s: ")
(ido-mode t)
(fido-mode t)

;; Line Number
(setq-default display-line-numbers-widen t)
(global-display-line-numbers-mode t)

;; Parentheses
(setq-default show-paren-when-point-inside-paren t
	      show-paren-when-point-in-periphery t)
(show-paren-mode t)

;; Recent Files
(setq-default recentf-max-menu-items 10
	      recentf-max-saved-items 10)
(add-hook 'kill-emacs-hook #'recentf-cleanup)
(recentf-mode 1)


(provide 'init-builtin)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here
