;;; init-package --- initialize the plugins

;;; Commentary:
;;; (c)Cabins, github.com/cabins/.emacs.d

;;; Code:

;; ******************** PART1 benchmark(Optional) ********************
;; Settings for benchmark package
(use-package benchmark-init
  :init (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate))




;; ******************** PART2 shell & environments ********************
;; Settings for org mode and load config from org file
(use-package org
  :config
  (setq org-startup-indented t)
  (when (display-graphic-p)
    (use-package org-bullets
      :hook (org-mode . (lambda() (org-bullets-mode 1))))))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/emacs-init.org"))

;; Settings for exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))




;; ******************** PART3 editing ********************
;; Settings for C-a behavior
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c C-k" . crux-kill-whole-line)))

;; Hungry Delete - delete multi spaces with one <delete> key
(use-package hungry-delete
  :bind (("C-c DEL" . hungry-delete-backward))
  :bind (("C-c d" . hungry-delete-forward)))

;; drag-stuff - move lines up/down
(use-package drag-stuff
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

;; Settings for company
(use-package company
  :config
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-modes t)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-other-buffers 'all)
  (setq company-require-match nil)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-tooltip-offset-display 'scrollbar)
  (setq company-begin-commands '(self-insert-command))
  :hook ((after-init . global-company-mode)))




;; ******************** PART4 searching ********************
;; Settings for ivy & counsel & swiper
(use-package ivy
  :defer 1
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "%d/%d "
        enable-recursive-minibuffers t
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (use-package ivy-posframe
    :delight
    :init
    (setq ivy-posframe-display-functions-alist
          '((swiper            . ivy-posframe-display-at-frame-center)
            (complete-symbol   . ivy-posframe-display-at-point)
            (counsel-M-x       . ivy-posframe-display-at-frame-center)
            (counsel-find-file . ivy-posframe-display-at-frame-center)
            (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
            (t                 . ivy-posframe-display-at-frame-center)))
    (ivy-posframe-mode 1)))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c C-f" . counsel-recentf)
         ("C-c g" . counsel-git)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-isearch-backward))
  :config (setq swiper-action-recenter t
                swiper-include-line-number-in-search t))




;; ******************** PART5 basic development ********************
;; Settings for which-key - suggest next key
(use-package which-key
  :defer nil
  :config (which-key-mode))

;; Settings for magit
;; I quit using magit on windows, 'cause its performance sucks
;; I use emacs builtin vc & cli-git on windows instead
(use-package magit
  :unless *is-windows*
  :bind ("C-x g" . magit-status))

;; Settings for yasnippet - not sure if works fine
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  ;; (yas-reload-all)
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (use-package yasnippet-snippets
    :defer t))

(use-package auto-yasnippet
  :bind (("C-o" . aya-open-line)
         ("H-w" . aya-create)
         ("H-y" . aya-expand)))

;; Settings for projectile
(use-package projectile
  :hook (after-init . projectile-mode)
  :config (setq-default projectile-mode-line-prefix " Proj")
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind ("C-c e" . flycheck-list-errors))

;; Settings for highlight parentheses
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

;; Settings for rainbow mode
;; (use-package rainbow-mode
;; :hook (prog-mode . rainbow-mode))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))



;; ******************** PART6 Emacs Optimize ********************
;; Settings for jump windows, use M-NUM to switch
(use-package ace-window
  :bind (("M-o" . 'ace-window)))

;; Settings for smooth scrolling
(use-package smooth-scrolling
  :init (setq smooth-scrolling-margin 2)
  :config (smooth-scrolling-mode t))

(provide 'init-package)
;;; init-package.el ends here
