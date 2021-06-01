;;; init-lisp.el --- Lisp settings
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

(use-package paredit
  :hook (emacs-lisp-mode . enable-paredit-mode))

(provide 'init-lisp)
;;; init-lisp.el ends here
