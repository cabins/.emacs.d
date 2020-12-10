;;; init-kbd.el --- Summary

;;; Commentary:
;;; (c)Cabins, github.com/cabins/.emacs.d

;;; Code:

;; 在macOS上，将Command键映射为Meta，Option映射为Super
(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; 在Windows上，将App/Menu键映射为Hyper
(when *is-windows*
  (setq w32-apps-modifier 'hyper))

;; Global key bind
(use-package emacs
  :bind (("M-/" . comment-line)
         ("M-?" . comment-or-uncomment-region)
         ("C-," . crux-find-user-init-file)
	     ("M-S-<return>" . toggle-frame-fullscreen)
         ("M-<return>" . toggle-frame-maximized)))

(provide 'init-kbd)
;;; init-kbd.el ends here
