;;; init-consts.el --- basic settings before Emacs startup

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

;; Consts to check operating system
(defconst *is-mac* (eq system-type 'darwin)
  "Const for system check, macOS.")

(defconst *is-linux* (eq system-type 'gnu/linux)
  "Const for system check, GNU/Linux.")

(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt))
  "Const for system check, Windows or DOS.")

(provide 'init-consts)
;;; init-consts.el ends here
