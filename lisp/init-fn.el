;;; init-fn.el --- define some useful interactive functions -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; a little bit optimize the screen display when in graphic mode
(defun cabins/optimize-screen ()
  "Optimize screen function."
  (when (display-graphic-p)
    ;; Keyboard scroll behavior
    (setq-default scroll-conservatively 100000
                  scroll-margin 3
                  scroll-preserve-screen-position t
		  ;; Mouse wheel scroll behavior
		  mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
		  mouse-wheel-progressive-speed nil)
    ;; Initialize the frame size
    ;; (set-frame-width (selected-frame) 130)
    ;; (set-frame-height (selected-frame) 40)
    ;; (toggle-frame-maximized)
    ))

(defun cabins/dark-modeline (dark-theme)
  "Customize the mode line style, DARK-THEME is boolean."

  (let ((colorb (if dark-theme "#2E3436" "#FFFFFF")))
    (dolist (name '(mode-line mode-line-inactive))
      (set-face-attribute name nil
			  :background colorb
			  :foreground "dimgray"
			  :box nil
			  :underline nil))))

(defun cabins/toggle-dark-theme ()
  "Toggle the theme to dark or light."
  (interactive)
  (if custom-enabled-themes
      (progn
        (disable-theme (car custom-enabled-themes))
        (cabins/dark-modeline nil))

    (load-theme 'tango-dark t)
    (cabins/dark-modeline t)))

(defun cabins/setup-font (f-en s-en f-cn s-cn)
  "The args mean:
F-EN font of English,
S-EN size of English,
F-CN font of Chinese,
S-CN size of Chinese."

  (when (display-graphic-p)
    (set-face-attribute 'default nil
		        :font (format "%s-%d" f-en s-en))

    (dolist (charset '(han cjk-misc chinese-gbk))
      (set-fontset-font "fontset-default" charset
                        (font-spec :family f-cn :size s-cn)))))

(provide 'init-fn)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-fn.el ends here
