;;; init-misc.el --- Summary
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

;; NOTE:
;; The style of use-package is NOT necessary
;; I write code with this style for unification

;; <TAB> show settings
(use-package tab-settings
  :ensure nil
  :init(setq-default tab-width 4
                     indent-tabs-mode nil))

;; C-s/C-r settings
(use-package search-settings
  :ensure nil
  :init (setq-default isearch-lazy-count t
                      lazy-count-prefix-format "%s/%s "))

;; some delete hooks settings
(use-package delete-settings
  :ensure nil
  :hook ((before-save-hook . delete-trailing-whitespace)
         (after-init . delete-selection-mode)))

;; settings for line number
(use-package line-number-settings
  :ensure nil
  :init
  (setq display-line-numbers-type 't) ; relative, visual
  (global-display-line-numbers-mode t))

;; recent files
(use-package recentf-settings
  :ensure nil
  :init
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (global-set-key (kbd "C-c f") 'recentf-open-files)
  (recentf-mode 1))

;; Cursor & Current Line
(use-package cursor-line-settings
  :ensure nil
  :init
  ;; blink the cursor
  (blink-cursor-mode 1)
  )

;; [built-in] Toggle hideshow minor mode on
(use-package hideshow-settings
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

(provide 'init-misc)
;;; init-misc.el ends here
