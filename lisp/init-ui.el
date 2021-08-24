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

(cabins/optimize-screen)

;; 字体设置（放弃了复杂的字体设置，直接使用特殊字体的方案）
;; 如果你也是使用中文的话（什么字体都行，下面的配对都能成功），推荐的几个配对：
;; Ubuntu Mono-11，对应中文11.0或者18
;; Couriew New-10，对应中文11.5或12.0或者20，这个配对在Win10上面不闪屏
;; 这两组数字在Windows 10 (13')测试通过，如果在你那里不合适，可尝试调整大小

(when *is-windows*
  (cabins/setup-font "Courier New" 10 "楷体" 11.5))

(when *is-mac*
  (cabins/setup-font "Courier New" 12 "华文楷体" 14.5))

;; 尝试解决字体卡顿问题
(setq inhibit-compacting-font-caches t)

;; reload the fonts & screen layout when in Daemon mode
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
              (cabins/optimize-screen))))

;; (load-theme 'deeper-blue t)

(provide 'init-ui)

;;; init-ui.el ends here
