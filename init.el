;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "lisp/")))

;; flymake cannot find load-path solution
;; [reference] https://emacs-china.org/t/flymake/8323/19
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

;; consts defination
(defconst *is-mac* (eq system-type 'darwin) "Apple macOS platform.")
(defconst *is-linux* (eq system-type 'gnu/linux) "GNU/Linux platform.")
(defconst *is-windows*
  (or (eq system-type 'ms-dos)
      (eq system-type 'windows-nt))
  "Windows / DOS.")

;; settings for independent packages and etc.
(require 'init-fn)
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


(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here
