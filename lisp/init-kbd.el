;;; init-kbd.el --- Summary

;;; Commentary:
;;; (c)Cabins, github.com/cabins/.emacs.d

;;; Code:

;; Settings for macOS key: Use command as the Meta key
(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; Global key bind
(use-package emacs
  :bind (("M-/" . comment-line)
         ("M-?" . comment-or-uncomment-region)
	     ("M-S-<return>" . toggle-frame-fullscreen)
         ("M-<return>" . toggle-frame-maximized)))

(provide 'init-kbd)
;;; init-kbd.el ends here
