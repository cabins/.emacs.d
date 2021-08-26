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
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook (after-init . global-company-mode)
  :config (setq company-minimum-prefix-length 1
                company-selection-wrap-around t))

;; Settings for which-key - suggest next key
(use-package which-key
  :defer nil
  :init (which-key-mode))

;; Settings for yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :bind (("C-o" . yas-expand))
  :config (use-package yasnippet-snippets :after yasnippet))

;; Settings for projectile
;; Using after-init hook makes emacs starts up faster than config projectile-mode
;; Disable it on Windows
(use-package projectile
  :unless *is-windows*
  :hook (after-init . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

;; Show the delimiters as rainbow color
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Beacon mode - highlight the line when the cursor jumps
(use-package beacon
  :hook (after-init . beacon-mode))

(provide 'init-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-package.el ends here
