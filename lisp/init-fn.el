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

(defun cabins/inhibit-bars ()
  "Inhibit the bars."

  (interactive)
  (when (display-graphic-p)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(defun cabins/optimize-screen ()
  "Optimize screen function."

  (when (display-graphic-p)
    ;; Keyboard scroll behavior
    (setq-default scroll-conservatively 100000
                  scroll-margin 3
		  scroll-step 1
                  scroll-preserve-screen-position t
		  scroll-up-aggressively 0.01
		  scroll-down-aggressively 0.01
		  auto-window-vscroll nil
		  ;; Mouse wheel scroll behavior
		  mouse-wheel-scroll-amount '(1 ((shift) . 1))
		  mouse-wheel-progressive-speed nil
		  mouse-wheel-follow-mouse 't
		  ;; Frame
		  frame-title-format '("%b"))
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
  (if (display-graphic-p)
      (if custom-enabled-themes
	  (progn
            (disable-theme (car custom-enabled-themes))
            (cabins/dark-modeline nil))

	(load-theme 'tango-dark t)
	(cabins/dark-modeline t))
    (message "Toggle dark theme only works on graphic mode.")))

(defun cabins/setup-font (fe se fc sc)
  "FE font of English, SE size of it; FC font of Chinese, SC size of it."

  (when (display-graphic-p)
    (set-face-attribute 'default nil
		        :font (format "%s-%d" fe se))

    (dolist (charset '(han cjk-misc chinese-gbk))
      (set-fontset-font "fontset-default" charset
                        (font-spec :family fc :size sc)))))

(defmacro cabins/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))


(provide 'init-fn)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-fn.el ends here
