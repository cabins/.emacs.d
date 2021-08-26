;;; init-startup.el --- configs for startup -*- lexical-binding: t -*-
;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

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

(setq locale-coding-system 'chinese-gbk	; 中文编码的设置（Linux平台待验证效果）
      make-backup-files nil             ; 备份文件的设置
      auto-save-default nil
      inhibit-startup-screen t          ; 开屏动画与提示信息的配置
      visible-bell nil)

;; macOS删除文件的时候删除到垃圾桶
(when *is-mac* (setq delete-by-moving-to-trash t))

(provide 'init-startup)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-startup.el ends here
