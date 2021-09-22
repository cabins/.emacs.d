;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Homepage: https://github.com/cabins

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp/")))
(when (and (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
	comp-deferred-compilation t
	package-native-compile t)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache" user-emacs-directory)))

;; flymake cannot find load-path solution
;; [reference] https://emacs-china.org/t/flymake/8323/19
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

;; consts defination
(defconst *is-mac* (eq system-type 'darwin) "Apple macOS platform.")
(defconst *is-linux* (eq system-type 'gnu/linux) "GNU/Linux platform.")
(defconst *is-windows* (memq system-type '(cygwin windows-nt ms-dos)) "Windows / DOS.")

;; settings for independent packages and etc.
(require 'init-fn)
(require 'init-startup)
(require 'init-elpa)
(require 'init-package)
(require 'init-builtin)
(require 'init-kbd)
(require 'init-lsp)
(require 'init-lang)
(require 'init-ui)

;; load custom file at last
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load-file custom-file))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
