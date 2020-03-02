;;; init-kbd.el --- Summary
;;; Commentary:
;;; Code:

;; Settings for macOS key: Use command as the Meta key
(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Global key bind
(use-package emacs
  :bind (("M-/" . comment-line)
         ("M-?" . comment-or-uncomment-region)))

(provide 'init-kbd)
;;; init-kbd.el ends here
