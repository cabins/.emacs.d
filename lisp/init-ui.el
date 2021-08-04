;;; init-ui.el --- settings for Emacs UI -*- lexical-binding: t -*-

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

(cabins/optimize-screen)

;; 字体设置（放弃了复杂的字体设置，直接使用特殊字体的方案）
(set-frame-font "Courier New")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "Microsoft Yahei" :size 16)))

;; 尝试解决字体卡顿问题
(setq inhibit-compacting-font-caches t)

;; reload the fonts & screen layout when in Daemon mode
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
              (cabins/optimize-screen))))

(load-theme 'deeper-blue t)

(provide 'init-ui)

;;; init-ui.el ends here
