;;; init.el --- Initialization file

;;; Commentary:
;;  This is NOT part of GNU Emacs.  It's a personal project of Emacs configuration.
;;  Written by (c) Cabins Kong.  2019-2020.

;;; Code:

(load-file (concat (file-name-directory user-emacs-directory)
                   "lisp/core/core-load-paths.el"))

(require 'init-before-start)
(require 'init-startup)
(require 'init-elpa)
(require 'init-package)
(require 'init-kbd)
(require 'init-misc)
(require 'init-program)

(require 'init-ui)

;;; init.el ends here
