;;; init-ui.el --- settings for the Emacs UI
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:


;; function to set monofonts
(defun cabins/set-monospaced-font (english chinese e-size c-size)
  ;; first, set the default latin charset
  (set-face-attribute 'default nil
                      :font (font-spec
                             :name english
                             :weight 'normal
                             :slant 'normal
                             :size e-size))

  ;; then, the cjk-charset
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec
                       :name chinese
                       :weight 'normal
                       :slant 'normal
                       :size c-size))))

;; a little bit optimize the screen display when in graphic mode
(defun cabins/optimize-screen ()
  (when (display-graphic-p)
    (setq-default cursor-type 'bar
                  scroll-up-aggressively 0.01
                  scroll-down-aggressively 0.01)
    (setq default-frame-alist '((width . 180) (height . 40))
          redisplay-dont-pause t
          scroll-conservatively 100000
          scroll-margin 0
          scroll-step 1
          scroll-preserve-screen-position 'always)))

(cabins/optimize-screen)

;; 尝试解决字体卡顿问题
(setq inhibit-compacting-font-caches t)

;; 添加图形界面这个是为了防止Deamon加载的时候中文字体设置失败
;; 其实可以不用判断系统，因为配置改成了一样的
;; 但是为了他人做系统的定制化，保留了系统的判断
(when (display-graphic-p)
  (if *is-windows*
      ;; font setting for Windows platform
      (cabins/set-monospaced-font "Courier New" "楷体" 13 16))
  (if *is-mac*
      ;; font setting for macOS platform
      (cabins/set-monospaced-font "Courier New" "楷体" 13 16))
  (if *is-linux*
      ;; font setting for GNU/Linux platform
      (cabins/set-monospaced-font "Courier New" "楷体" 13 16))
  )

;; 解决Deamon启动的时候，字体不能加载的问题
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
              (cabins/set-monospaced-font "Courier New" "楷体" 13 16)
              (cabins/optimize-screen))))

(require 'init-modeline)

(provide 'init-ui)

;;; init-ui.el ends here
