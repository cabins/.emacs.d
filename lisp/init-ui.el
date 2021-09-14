;;; init-ui.el --- settings for Emacs UI -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

(cabins/optimize-screen)
(cabins/inhibit-bars)

;; 从Deamon模式加载窗口需要重新加载一次
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)(cabins/optimize-screen))))

;; 字体设置，以下配置中中文任意字体均可，推荐配对：
;; Ubuntu Mono-11 / 中文11.0或者18
;; Couriew New-10 / 中文11.5或12.0或者20
(when *is-windows* (cabins/setup-font "Courier New" 11 "楷体" 13.0))
(when *is-mac* (cabins/setup-font "Courier New" 12 "华文楷体" 14.5))

;; 默认使用白色Modeline，视觉上更轻量
(cabins/dark-modeline nil)

(provide 'init-ui)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ui.el ends here
