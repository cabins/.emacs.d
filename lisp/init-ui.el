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
;; reload the fonts & screen layout when in Daemon mode
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)(cabins/optimize-screen))))

;; 字体设置（放弃了复杂的字体设置，直接使用特殊字体的方案）
;; 如果你也是使用中文的话（什么字体都行，下面的配对都能成功），推荐的几个配对：
;; Ubuntu Mono-11，对应中文11.0或者18
;; Couriew New-10，对应中文11.5或12.0或者20，这个配对在Win10上面不闪屏
;; 这两组数字在Windows 10 (13')测试通过，如果在你那里不合适，可尝试调整大小
(when *is-windows* (cabins/setup-font "Courier New" 10 "楷体" 11.5))
(when *is-mac* (cabins/setup-font "Courier New" 12 "华文楷体" 14.5))

;; 尝试解决字体卡顿问题
(setq inhibit-compacting-font-caches t)

;; 修改Modeline的颜色，使其看起来更轻量
(cabins/dark-modeline nil)

(provide 'init-ui)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ui.el ends here
