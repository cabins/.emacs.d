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
(if (and (display-graphic-p) (eq system-type 'darwin))
    (menu-bar-mode 1)
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-maximized)

;; adjust the fonts
(defun get-font-available (font-list)
  "Get the first available font from FONT-LIST."
  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	  (throw 'font font)))))

(defun cabins/setup-font ()
  "Font setup."

  (setq enfonts '("Cascadia Code"	; Windows 10
		  "Source Code Pro"	; Common
		  "Consolas"		; Windows
		  "Courier New"		; Windows or macOS
		  "Ubuntu Mono"		; Ubuntu
		  "Monaco"		; macOS
		  ))
  (setq cnfonts '("STKaiti"		; macOS
		  "华文楷体"		; Windows
		  "STHeiti"		; macOS
		  "微软雅黑"		; Windows
		  "华文黑体"		; maybe macOS
		  "文泉驿微米黑"	; GNU/Linux
		  ))

  (let ((cnfont (get-font-available cnfonts))
	(enfont (get-font-available enfonts)))
    (if enfont
	(set-face-attribute 'default nil
			    :font (format "%s-%d" enfont 9))
      (message "Failed to set default font."))
    (if cnfont
	(dolist (charset '(kana han cjk-misc bopomofo chinese-gbk))
	  (set-fontset-font "fontset-default" charset
			    (font-spec :family cnfont :size 11.0)))
      (message "Failed to set CJK font."))))

(cabins/setup-font)

;; theme settings
(load-theme 'leuven)

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
