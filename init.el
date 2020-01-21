;;; init.el --- Initialization file
;;; Commentary:

;;  This is NOT part of GNU Emacs.  It's a personal project of Emacs configuration.
;;  Written by Cabins Kong.  2019-2020.

;;; Code:

;; Settings for the lisp config path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Import the consts
(require 'init-consts)

;; Settings before startup
(require 'init-startup)

;; Initialize the packages
(require 'init-package)

;; UI Settings
(require 'init-ui)

;; Keyboard settings
(require 'init-kbd)

;; Misc settings
(require 'init-misc)

;; Programming Settings
(require 'init-program)

;; Settings for web mode
;; (require 'init-web)

;; Settings for customize functions
(require 'init-funcs)

;;; init.el ends here
