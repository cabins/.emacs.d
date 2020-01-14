;;; init-package --- initialize the plugins
;;; Commentary:
;;; Code:

;;; Package initialize
(when (< emacs-major-version 27)
  (package-initialize)
  )

(unless package-archive-contents
  (package-refresh-contents))


;;; Settings for use-package package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;;; Settings for exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

;;; Settings for C-a behavior
(use-package crux
  :defer t
  :bind (("C-a" . crux-move-beginning-of-line))
  )

;;; Hungry Delete - delete multi spaces with one <delete> key
(use-package hungry-delete
  :defer nil
  :bind (("C-c DEL" . hungry-delete-backward))
  :bind (("C-c C-d" . hungry-delete-forward))
  )


;;; Settings for ivy & counsel & swiper
(use-package ivy
  :defer 1
  :demand
  :config
  (progn
    (setq ivy-use-virtual-buffers t
	  ivy-count-format "%d / %d ")
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
    )
  )
(use-package counsel
  :defer 1
  :after (ivy))
(use-package swiper :defer 1)

;;; Settings for company
(use-package company
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  :hook ((after-init . global-company-mode))
  )

;;; Settings for which-key - suggest next key
(use-package which-key
  :config
  (which-key-mode +1)
  )

;;; Settings for magit
(use-package magit
  :defer t
  :bind ("C-x g" . magit-status)
  )

;;; Settings for yasnippet - not sure if works fine
(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  ;; (yas-reload-all)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (use-package yasnippet-snippets
    :defer t)
  )

(use-package auto-yasnippet
  :init
  (global-set-key (kbd "C-o") #'aya-open-line)
  (global-set-key (kbd "H-w") #'aya-create)
  (global-set-key (kbd "H-y") #'aya-expand)
  )

;;; Settings for projectile
(use-package projectile
  :defer t
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (global-set-key (kbd "C-c C-p") 'find-file-in-project)
    (projectile-mode +1)
    ))

(use-package flycheck
  :defer nil
  :init (global-flycheck-mode)
  :bind (
	 ("M-n" . 'flycheck-next-error)
	 ("M-p" . 'flycheck-previous-error))
  )

;;; Settings for jump windows, use M-NUM to switch
(use-package window-numbering
  :defer nil
  :config
  (window-numbering-mode t))

;;; Settings for highlight parentheses
(use-package highlight-parentheses
  :defer t
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  )

;;; Settings for rainbow mode
(use-package rainbow-mode
  :defer t
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;;; drag-stuff - move lines up/down
(use-package drag-stuff
  :bind (
	 ("<M-up>". drag-stuff-up)
	 ("<M-down>" . drag-stuff-down))
  )

;;; Settings for treemacs
(use-package treemacs
  :defer t
  :bind (("C-c t" . treemacs))
  )
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit)

(provide 'init-package)
;;; init-package.el ends here
