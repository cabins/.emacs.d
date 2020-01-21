;;; init-misc.el --- Summary
;;; Commentary:
;;; Code:

;; Settings for delete multi line spaces
(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-misc)
;;; init-misc.el ends here.
