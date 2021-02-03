;;; init-python.el --- Python settings
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

(defvar python--tools '("python-language-server[all]"
                        "black"
                        "isort")
  "Modules for Python development")

(defun cabins/python-update-tools ()
  "Install or update modules for Python development."
  (interactive)

  ;; Check the pip is installed correctly
  (unless (executable-find "pip")
    (user-error "Pls make sure that pip is installed correctly."))

  ;; Install or update python modules
  (let ((proc-name "python-tools")
        (proc-buffer "*Python Modules*"))
    (dolist (module python--tools)
      (set-process-sentinel (start-process proc-name proc-buffer "pip" "install" "-U" module) nil))))

(setq-default python-indent-offset 4
              python-indent-guess-indent-offset-verbose nil)

;; Use black to format the Python code
(use-package blacken
  :hook ((python-mode . blacken-mode)))

;; Sort the pytho imports
(use-package py-isort
  :init (setq python-sort-imports-on-save t))

(provide 'init-python)
;;; init-python.el ends here
