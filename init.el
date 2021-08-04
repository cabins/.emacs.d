;;; init.el --- Initialization file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is NOT part of GNU Emacs.  It's a personal project of Emacs configuration  ;;
;; Written by (c) Cabins Kong.  2019-2021.                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp/")))

;; flymake cannot find load-path solution
;; [reference] https://emacs-china.org/t/flymake/8323/19
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

;; consts defination
(defconst *is-mac* (eq system-type 'darwin) "macOS platform.")
(defconst *is-linux* (eq system-type 'gnu/linux) "GNU/Linux platform.")
(defconst *is-windows*
  (or (eq system-type 'ms-dos)
      (eq system-type 'windows-nt))
  "Windows / DOS.")

;; settings for independent packages and etc.
(require 'init-funcs)
(require 'init-startup)
(require 'init-elpa)
(require 'init-package)
(require 'init-builtin)
(require 'init-kbd)
(require 'init-misc)
(require 'init-program)
(require 'init-ui)

;; load custom file at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

;;; init.el ends here
