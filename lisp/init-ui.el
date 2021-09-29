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

;; settings for scrolling
(setq-default scroll-step 1
	      scroll-preserve-screen-position t
	      scroll-up-aggressively 0.01
	      scroll-down-aggressively 0.01
	      redisplay-dont-pause t
	      auto-window-vscroll nil
	      ;; Mouse wheel scroll behavior
	      mouse-wheel-scroll-amount '(1 ((shift) . 1))
	      mouse-wheel-progressive-speed nil
	      mouse-wheel-follow-mouse 't
	      fast-but-imprecise-scrolling nil)

(when (display-graphic-p)
  ;; Initialize the frame size
  ;; (set-frame-width (selected-frame) 130)
  ;; (set-frame-height (selected-frame) 40)
  (toggle-frame-maximized))

(cabins/inhibit-bars)

;; 字体设置，以下配置中中文任意字体均可，推荐配对：
;; Ubuntu Mono-11 / 中文11.0或者18
;; Couriew New-10 / 中文11.5或12.0或者20
(when *is-windows* (cabins/setup-font "Consolas" 10 "华文楷体" 18))
(when *is-mac* (cabins/setup-font "Courier New" 12 "华文楷体" 14.5))

;; 默认使用白色Modeline，视觉上更轻量
(when (display-graphic-p)
  (cabins/dark-modeline nil))

;; 从Deamon模式加载窗口需要重新加载一次
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
	      (cabins/dark-modeline nil)
	      )))

(provide 'init-ui)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ui.el ends here
