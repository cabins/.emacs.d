;;; init-lisp.el --- Lisp settings

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

(use-package lispy
  :hook
  (emacs-lisp-mode . (lambda () (lispy-mode 1)))
  (lisp-interaction-mode . (lambda ()(lispy-mode 1))))

(provide 'init-lisp)
;;; init-lisp.el ends here
