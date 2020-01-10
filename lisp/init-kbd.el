;;; init-kbd.el --- Summary
;;; Commentary:


;;; Code:

;;; Replace yes/no answer with y/n
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "M-/") 'comment-line)
(global-set-key (kbd "M-?") 'comment-or-uncomment-region)




(provide 'init-kbd)
;;; init-kbd.el ends here.
