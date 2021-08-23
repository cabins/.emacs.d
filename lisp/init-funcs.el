;;; init-funcs.el --- define some useful interactive functions -*- lexical-binding: t -*-

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

;; a little bit optimize the screen display when in graphic mode
(defun cabins/optimize-screen ()
  "Optimize screen function."
  (when (display-graphic-p)
    (setq-default cursor-type 'bar
                  scroll-up-aggressively 0.01
                  scroll-down-aggressively 0.01)
    (setq scroll-conservatively 100000
          scroll-margin 0
          scroll-step 1
          scroll-preserve-screen-position 'always)
    (set-frame-width (selected-frame) 130)
    (set-frame-height (selected-frame) 40)))

(defun cabins/user-login-info ()
  "Print the login user info as init message."
  (let ((prefix ";; Configured by Cabins <github.com/cabins>.\n")
        (os-version (format ";; %20s: %S\n" "Operating System" system-type))
        (user-names (format ";; %20s: %s\n" "Login User" (user-login-name)))
        (machine-name (format ";; %20s: %s\n" "Machine Name" (system-name)))
        (suffix ";; Enjoy!"))
    (concat prefix ";;\n" os-version user-names machine-name ";;\n" suffix)))

(defun cabins/toggle-dark-theme ()
  "Toggle the theme to dark or light."
  (interactive)
  (if custom-enabled-themes
      (disable-theme (car custom-enabled-themes))
    (load-theme 'deeper-blue t)))

(defun cabins/setup-font (font)
  "The FONT arg is dot pair list, eg. '((\"Courier New\" . 10)(\"黑体\" . 12.0))."
  ;; Default font
  (when (display-graphic-p)
    (set-face-attribute 'default nil
		                :font (format "%s-%d" (caar font) (cdar font)))

    (dolist (charset '(han cjk-misc chinese-gbk))
      (set-fontset-font "fontset-default" charset
                        (font-spec
                         :family (car (cadr font))
                         :size (cdr (cadr font)))))) )


(provide 'init-funcs)

;;; init-funcs.el ends here
