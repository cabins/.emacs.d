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

;; disable the bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-maximized)

;; adjust the fonts size, pls
(defun cabins/setup-font ()
  "Font setup."

  (set-face-attribute 'default nil
		      :font (format "%s-%d" "Source Code Pro" 10))

  (dolist (charset '(han cjk-misc chinese-gbk))
    (set-fontset-font "fontset-default" charset
                      (font-spec :family "华文楷体" :size 12.5))))
(cabins/setup-font)

;; theme settings
(load-theme 'dichromacy)

;; settings for daemon mode
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
	      (toggle-frame-maximized)
	      (cabins/setup-font))))

(provide 'init-ui)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-ui.el ends here
