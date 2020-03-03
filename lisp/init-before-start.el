;;; init-before-start.el --- basic settings before Emacs startup

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

;; (setq default-frame-alist '((width . 150) (height . 35))
;; (set-frame-parameter nil 'fullscreen 'maximized)

;; Consts to check operating system
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-windows* (or (eq system-type 'ms-dos) (eq system-type 'windows-nt)))

(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  ;; Display menu bar on macOS with GUI mode (Global Menu)
  (unless *is-mac*
    (menu-bar-mode -1)))

(setq inhibit-startup-screen t
      initial-buffer-choice nil)

(provide 'init-before-start)
;;; init-before-start.el ends here
