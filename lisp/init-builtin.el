;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; Use ido instead of ivy & counsel & swiper
;; They are great! But I want cleaner.
(use-package ido
  :defer nil
  :init (setq ido-enable-flex-matching t
              ido-everywhere t
              ido-use-filename-at-point t)
  (ido-mode t)
  (fido-mode t))

;; Enable flymake on default, which is built in emacs
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

;; Show parentheses
(use-package paren :init (show-paren-mode 1))

;; Settings for electric-pair
(use-package electric
  :hook ((after-init . electric-indent-mode)
	 (prog-mode . electric-pair-mode)))

;; Abbrev mode
(use-package abbrev
  :ensure nil
  :init (setq-default abbrev-mode t))

;; display 'lambda' as 'λ' (just for fun)
;; (use-package prettify-symbols-mode
;;   :ensure nil
;;   :init (global-prettify-symbols-mode 1))

;; settings for windmove, replace the ace-window plugin
(use-package windmove
  :ensure nil
  :bind (("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)))

(provide 'init-builtin)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here
