;;; init-package --- initialize the plugins
;;; Commentary: (c)Cabins, github.com/cabins/.emacs.d
;;; Code:

;; ******************** benchmark (Optional) ********************
;; Settings for benchmark package
(use-package benchmark-init
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;; for exec-path-from-shell
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

;; Settings for company
(use-package company
  :diminish (company-mode " Cmp.")
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook (after-init . global-company-mode)
  :config (setq company-dabbrev-code-everywhere t
		        company-dabbrev-code-modes t
		        company-dabbrev-code-other-buffers 'all
		        company-dabbrev-downcase nil
		        company-dabbrev-ignore-case t
		        company-dabbrev-other-buffers 'all
		        company-require-match nil
		        company-minimum-prefix-length 1
		        company-show-numbers t
		        company-tooltip-limit 20
		        company-idle-delay 0
		        company-echo-delay 0
		        company-tooltip-offset-display 'scrollbar
		        company-begin-commands '(self-insert-command))
  (eval-after-load 'company
    '(add-to-list 'company-backends
                  '(company-abbrev company-yasnippet company-capf))))

;; Better sorting and filtering
(use-package company-prescient
  :init (company-prescient-mode 1))

;; [built-in] Use ido instead of ivy & counsel & swiper
;; They are great! But I want cleaner.
(use-package ido
  :defer nil
  :init
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-use-filename-at-point t)
  (ido-mode t)
  (if (< emacs-major-version 27)
      (icomplete-mode t)
    (fido-mode t)))

;; Settings for which-key - suggest next key
(use-package which-key
  :defer nil
  :config (which-key-mode))

;; Settings for yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config
  (add-to-list 'yas-snippet-dirs (concat
				                  (file-name-directory user-emacs-directory)
				                  "snippets"))
  (use-package yasnippet-snippets
    :after yasnippet)

  (use-package auto-yasnippet
    :bind (("C-o" . aya-open-line)
           ("H-w" . aya-create)
           ("H-y" . aya-expand))))

;; Settings for projectile
;; Using after-init hook makes emacs starts up faster than config projectile-mode
(use-package projectile
  :unless *is-windows*
  :diminish (projectile-mode " Proj.")
  :hook (after-init . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))

;; [built-in] Enable flymake on default, which is built in emacs
(use-package flymake
  :ensure nil
  :diminish (flymake " Flym.")
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

;; Settings for highlight parentheses
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

;; Beacon mode - highlight the line where your cursor is
(use-package beacon
  :unless *is-windows*
  :hook (after-init . beacon-mode))

;; Indent grade guide line
(use-package indent-guide
  :hook (after-init . indent-guide-global-mode))

;; [built-in] paren mode
(use-package paren
  :ensure nil
  :config (show-paren-mode 1))

;; [built-in] electric-pair
(use-package electric
  :ensure nil
  :hook ((after-init . electric-indent-mode)
	     (prog-mode . electric-pair-mode)))

(provide 'init-package)
;;; init-package.el ends here
