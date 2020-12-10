;;; interactive-funcs.el --- define some useful interactive-funcs

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

;; DEPENDENCIES

(defun create-quick-config-link (label link)
  (insert label ": ")
  (insert-button link
                 'action (lambda (_) (find-file link))
                 'follow-link t)
  (insert "\n"))

;; INTERACTIVE FUNCS
(defun open-common-config-files ()
  (interactive)
  (let ((buf (get-buffer-create "*Config Links*"))
        (configs '(("Emacs" . "~/.config/emacs.d"))))
    (with-current-buffer buf
      (erase-buffer)
      (mapcar (lambda (item)
                (create-quick-config-link (car item) (cdr item)))
              configs))
    (pop-to-buffer buf t)))

(defun reload-init-file ()
  "Reload the user init file."
  (interactive)
  (load-file user-init-file))

(provide 'interactive-funcs)
;;; interactive-funcs.el ends here
