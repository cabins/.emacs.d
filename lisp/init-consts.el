;;; init-consts.el --- Define some consts used in the context
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

;; Settings for macOS key: Use command as the Meta key
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

(provide 'init-consts)
;;; init-consts.el ends here
