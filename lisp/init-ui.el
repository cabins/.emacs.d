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
    (when enfont
      (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
	(set-face-attribute face nil
			    :family enfont)))

    (when cnfont
      (dolist (charset '(kana han cjk-misc bopomofo))
	(set-fontset-font t charset cnfont))
      (setq face-font-rescale-alist
	    (mapcar (lambda (item)
		      (cons item 1.2))
		    cnfonts)))))

;; settings for daemon mode
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(with-selected-frame frame
		  (cabins/setup-font))))
  (add-hook 'after-init-hook #'cabins/setup-font))

(provide 'init-ui)

;;; init-ui.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
