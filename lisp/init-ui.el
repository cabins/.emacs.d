;;; init-ui.el --- settings for the Emacs UI
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

;; a little bit optimize the screen display when in graphic mode
(defun cabins/optimize-screen ()
  "Optimize screen function."
  (when (display-graphic-p)
    (setq-default cursor-type 'bar
                  scroll-up-aggressively 0.01
                  scroll-down-aggressively 0.01)
    (setq scroll-conservatively 100000
          scroll-margin 0
          scroll-step 1
          scroll-preserve-screen-position 'always)
    (set-frame-width (selected-frame) 130)
    (set-frame-height (selected-frame) 40)))

(cabins/optimize-screen)

;; 字体设置（放弃了复杂的字体设置，直接使用特殊字体的方案）
(set-frame-font "Courier New")
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "Microsoft Yahei" :size 16)))

;; 尝试解决字体卡顿问题
(setq inhibit-compacting-font-caches t)

;; reload the fonts & screen layout when in Daemon mode
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
              (cabins/optimize-screen))))

(defun cabins/toggle-dark-theme ()
  "Toggle the theme to dark or light."
  (interactive)
  (if custom-enabled-themes
      (disable-theme (car custom-enabled-themes))
    (load-theme 'deeper-blue t)))

(load-theme 'deeper-blue t)
(provide 'init-ui)

;;; init-ui.el ends here
