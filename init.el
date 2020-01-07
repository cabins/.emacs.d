;;; init.el --- Summary
;;; Commentary:

;;; Code:
;;; Settings for the lisp config path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; Settings before startup
(require 'init-startup)

;;; Initialize the packages
(require 'init-package)

;;; UI Settings
(require 'init-ui)

;;; Keyboard settings
(require 'init-kbd)

;;; Misc settings
(require 'init-misc)

;;; ============ Programming Settings ============
;;; Settings for lsp (include golang & python)
;;(require 'init-golang)
;;(require 'init-python)
(require 'init-program)
;;; Settings for web mode
(require 'init-web)

;;; init.el ends here
