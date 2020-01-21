;;; init-package --- initialize the plugins
;;; Commentary:
;;; Code:

(setq package-check-signature nil)

(require 'package)
(setq package-enable-at-startup t)

;;; Settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;;; Package initialize
;; (when (< emacs-major-version 27)
  ;; (package-initialize))

;;; Initialize the packages, avoiding a re-initialization
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;;; Settings for use-package package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Configure use-package prior to loading it
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-verbose t))

(eval-and-compile
  (require 'use-package))

;;; Settings for benchmark package
(use-package benchmark-init
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))

;;; Settings for org mode and load config from org file
(use-package org
  :defer t
  :config
  (setq org-startup-indented t)
  (when (display-graphic-p)
    (use-package org-bullets
    :hook (org-mode . (lambda() (org-bullets-mode 1))))))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))

;;; Settings for exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; Settings for C-a behavior
(use-package crux
  :defer t
  :bind (("C-a" . crux-move-beginning-of-line)))

;;; Hungry Delete - delete multi spaces with one <delete> key
(use-package hungry-delete
  :defer nil
  :bind (("C-c DEL" . hungry-delete-backward))
  :bind (("C-c d" . hungry-delete-forward)))

;;; Settings for ivy & counsel & swiper
(use-package ivy
  :defer 1
  :demand
  :config
  (setq ivy-use-virtual-buffers t
	ivy-initial-inputs-alist nil
	ivy-count-format "%d/%d "
	enable-recursive-minibuffers t
	ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :hook (after-init . ivy-mode))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c g" . counsel-git)))

(use-package swiper
  :after ivy
  :config (setq swiper-action-recenter t
		swiper-include-line-number-in-search t)
  :bind ("C-s" . swiper))

;;; Settings for company
(use-package company
  :config
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-modes t)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-other-buffers 'all)
  (setq company-require-match nil)
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-tooltip-offset-display 'scrollbar)
  (setq company-begin-commands '(self-insert-command))
  :hook ((after-init . global-company-mode)))

;;; Settings for which-key - suggest next key
(use-package which-key
  :config (which-key-mode +1))

;;; Settings for magit
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

;;; Settings for yasnippet - not sure if works fine
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  ;; (yas-reload-all)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (use-package yasnippet-snippets
    :defer t))

(use-package auto-yasnippet
  :defer t
  :init
  (global-set-key (kbd "C-o") #'aya-open-line)
  (global-set-key (kbd "H-w") #'aya-create)
  (global-set-key (kbd "H-y") #'aya-expand))

;;; Settings for projectile
(use-package projectile
  :hook (after-init . projectile-mode)
  :config (setq-default projectile-mode-line-prefix " Proj")
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package flycheck
  :init (global-flycheck-mode)
  :bind (("M-n" . 'flycheck-next-error)
	 ("M-p" . 'flycheck-previous-error)))

;;; Settings for jump windows, use M-NUM to switch
(use-package ace-window
  :bind (("M-o" . 'ace-window)))

;;; Settings for highlight parentheses
(use-package highlight-parentheses
  :defer t
  :hook (prog-mode . highlight-parentheses-mode))

;;; Settings for rainbow mode
(use-package rainbow-mode
  :defer t
  :hook (prog-mode . rainbow-mode))

;;; drag-stuff - move lines up/down
(use-package drag-stuff
  :bind (("<M-up>". drag-stuff-up)
	 ("<M-down>" . drag-stuff-down)))

;;; Settings for treemacs
;; (use-package treemacs
  ;; :defer t
  ;; :bind (("C-c t" . treemacs)))

;; (use-package treemacs-projectile
  ;; :after treemacs projectile)

;; (use-package treemacs-magit
  ;; :after treemacs magit)

;;; Settings for smooth scrolling
(use-package smooth-scrolling
  :init (setq smooth-scrolling-margin 3)
  :config (smooth-scrolling-mode t))

(provide 'init-package)
;;; init-package.el ends here
