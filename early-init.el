;;; early-init.el --- Emacs 27 introduces early-init.el, which runs before init.el
;;; Commentary:
;; Runs before package and UI initializetion happens.
;;; Code:

;; Do not initialize the package manager, which is done in 'init.el'
(setq package-enable-at-startup nil)

;;; early-init.el ends here
