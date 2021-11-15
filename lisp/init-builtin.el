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
;;; Sorted by Alphbet order

;; Abbrev
(setq-default abbrev-mode t)

;; auto save
;; `save-some-buffers' is provided by files.el (builtin)
;; `pulse-momentary-highlight-one-line' is provided by pulse.el (builtin)
(use-package pulse-and-save
  :ensure nil
  :init
  (defun pulse-save-buffers (&rest args)
    (save-some-buffers t)
    (pulse-momentary-highlight-one-line (point)))
  ;; auto save when frame lose focus, Alt-Tab
  (add-function :after after-focus-change-function #'pulse-save-buffers)
  ;; auto save when buffer changed
  (dolist (command '(other-window
                     switch-to-buffer
                     next-buffer
                     previous-buffer))
    (advice-add command :after #'pulse-save-buffers)))

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

;; Delete Behavior
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Electric-Pair
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-layout-mode)

;; Flymake
(add-hook 'prog-mode-hook 'flymake-mode)

;; HideShow Minor Mode
(use-package hideshow
  :init (add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))
  :hook (prog-mode . hs-minor-mode))

;; ibuffer
(use-package ibuffer
  :init (defalias 'list-buffers 'ibuffer))

;; Ido ( instead of ivy & counsel & swiper)
(setq-default ido-auto-merge-work-directories-length -1
	      ido-enable-flex-matching t
	      isearch-lazy-count t
	      lazy-count-prefix-format "%s/%s: ")
(setq completion-ignored-extensions '(".o" ".elc" "~" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".class"))
(fido-mode t)

;; Line Number
;; this package introduced in Emacs 26, so only enabled when 26+
(use-package display-line-numbers
  :if (> emacs-major-version 26)
  :hook (prog-mode . display-line-numbers-mode))

;; Org Mode
(setq org-hide-leading-stars t
      org-startup-indented t)

;; Parentheses
(use-package paren
  :ensure nil
  :config (setq-default show-paren-style 'mixed
                        show-paren-when-point-inside-paren t
                        show-paren-when-point-in-periphery t)
  :hook (prog-mode . show-paren-mode))

;; Recent Files
(add-hook 'after-init-hook (lambda ()
			     (recentf-mode 1)
			     (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/"))))
(setq-default recentf-max-menu-items 20
	      recentf-max-saved-items 20)

;; Save Place
(save-place-mode 1)

;; only use spaces instead of TAB, use C-q TAB to input the TAB char
(setq-default indent-tabs-mode nil)

;; Diminish Builtins
(dolist (elem '(abbrev-mode eldoc-mode))
  (diminish elem))
(add-hook 'hs-minor-mode-hook (lambda () (diminish 'hs-minor-mode)))

(provide 'init-builtin)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-builtin.el ends here
