;;; init-startup.el --- configs for startup -*- lexical-binding: t -*-

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

;; 系统编码设置
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8)
;; Windows系统特殊配置
(when (eq system-type 'windows-nt)
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))

(setq locale-coding-system 'chinese-gbk
      make-backup-files nil             ; 备份文件的设置
      auto-save-default nil
      inhibit-startup-screen t          ; 开屏动画与提示信息的配置
      initial-scratch-message (cabins/user-login-info)
      ring-bell-function #'ignore       ; 是否响铃的设置
      visible-bell nil)

;; macOS删除文件的时候删除到垃圾桶
(when *is-mac*
    (setq delete-by-moving-to-trash t))


(provide 'init-startup)

;;; init-startup.el ends here
