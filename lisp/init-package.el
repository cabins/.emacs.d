;;; init-package.el --- initialize the plugins -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:

;; (c) Cabins Kong, 2020-2021

;;; Code:

;; Settings for exec-path-from-shell
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; Settings for company
(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (setq company-minimum-prefix-length 1
                company-selection-wrap-around t))

;; Settings for which-key - suggest next key
(use-package which-key
  :defer nil
  :diminish
  :init (which-key-mode))

;; Settings for yasnippet
(use-package yasnippet
  :hook (after-init . yas-global-mode))
(use-package yasnippet-snippets)

;; Settings for projectile (Use builtin project in Emacs 28)
(use-package projectile
  :when (< emacs-major-version 28)
  :diminish " Proj."
  :init (add-hook 'after-init-hook 'projectile-mode)
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Show the delimiters as rainbow color
(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Beacon mode - highlight the line when the cursor jumps
(use-package beacon
  :diminish
  :unless *is-windows*
  :init (add-hook 'after-init-hook 'beacon-mode))

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
