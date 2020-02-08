;;; init.el --- Initialization file
;;; Commentary:

;;  This is NOT part of GNU Emacs.  It's a personal project of Emacs configuration.
;;  Written by Cabins Kong.  2019-2020.

;;; Code:

;; Settings for the lisp config path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Settings for Emacs
(require 'init-consts)
(require 'init-startup)
(require 'init-elpa)
(require 'init-package)
(require 'init-ui)
(require 'init-kbd)
(require 'init-misc)
(require 'init-program)
;; (require 'init-web)
(require 'init-funcs)

;;; init.el ends here
