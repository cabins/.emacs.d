;;; init-builtin the plugins
;;; Commentary: (c)Cabins, github.com/cabins/.emacs.d
;;; Code:

;; Settings for org mode and load config from org file
(use-package org
  ;; :init (setq org-startup-indented t)
  :config
  (setq org-startup-indented t
	    org-todo-keywords '((sequence "TODO" "DOING" "DONE"))
	    org-todo-keyword-faces '(("DOING" . "blue"))))

;; Use ido instead of ivy & counsel & swiper
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

;; Enable flymake on default, which is built in emacs
(use-package flymake
  :ensure nil
  :diminish (flymake " Flym.")
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package paren
  :config (show-paren-mode 1))

;; Settings for electric-pair
(use-package electric
  :hook ((after-init . electric-indent-mode)
	     (prog-mode . electric-pair-mode)))

;; Abbrev mode
(use-package abbrev-mode
  :ensure nil
  :init (abbrev-mode 1))

;; display 'lambda' as 'λ' (just for fun)
(use-package prettify-symbols-mode
  :ensure nil
  :init (global-prettify-symbols-mode 1))

(provide 'init-builtin)
;;; init-builtin.el ends here
