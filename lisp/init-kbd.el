;;; init-kbd.el --- Summary
;;; Commentary:


;;; Code:

;; Replace yes/no answer with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Settings for comment/uncomment
(global-set-key (kbd "M-/") 'comment-line)
(global-set-key (kbd "M-?") 'comment-or-uncomment-region)

;; Settings for kill one whole line
(global-set-key (kbd "C-c C-k") 'crux-kill-whole-line)

;; Settings for macOS key: Use command as the Meta key
(defconst *is-a-mac* (eq system-type 'darwin))
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(provide 'init-kbd)
;;; init-kbd.el ends here
