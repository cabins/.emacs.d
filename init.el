;;; init.el --- Initialization file
;;; Commentary:

;;  This is NOT part of GNU Emacs.  It's a personal project of Emacs configuration.
;;  Written by Cabins Kong.  2019-2020.

;;; Code:

;; I Like maximize the frame window
;; Comment it if you do NOT like this.
(when (display-graphic-p)
  ;; Init GUI size as maximized mode
  (set-frame-parameter nil 'fullscreen 'maximized)
    ;;; If you don't want maximized window, you can use the next line to define a customized size
  ;; (setq default-frame-alist '((width . 150) (height . 35)))
  )

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
(require 'init-funcs)

;;; init.el ends here
