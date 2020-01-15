;;; init-funcs.el --- Init some functions here
;;; Commentary:

;;; Code:

(defun cabins/open-emacs-init-file ()
  "Open the emacs config file"
  (interactive)
  (find-file user-init-file)
  )

(global-set-key (kbd "C-c ,") 'cabins/open-emacs-init-file)



(provide 'init-funcs)
;;; init-funcs.el ends here
