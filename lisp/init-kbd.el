;;; init-kbd.el --- Summary
;;; Commentary:
;;; Code:

;; Settings for comment/uncomment
(global-set-key (kbd "M-/") 'comment-line)
(global-set-key (kbd "M-?") 'comment-or-uncomment-region)

;; Settings for macOS key: Use command as the Meta key
(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(provide 'init-kbd)
;;; init-kbd.el ends here
