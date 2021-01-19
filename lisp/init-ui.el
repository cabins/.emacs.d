;;; init-ui.el --- settings for the Emacs UI
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

;; a little bit optimize the screen display when in graphic mode
(defun cabins/optimize-screen ()
  (when (display-graphic-p)
    (setq-default cursor-type 'bar
                  scroll-up-aggressively 0.01
                  scroll-down-aggressively 0.01)
    (setq redisplay-dont-pause t
          scroll-conservatively 100000
          scroll-margin 0
          scroll-step 1
          scroll-preserve-screen-position 'always)
    (set-frame-width (selected-frame) 90)
    (set-frame-height (selected-frame) 50)))

(cabins/optimize-screen)

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

;; 尝试解决字体卡顿问题
(setq inhibit-compacting-font-caches t)

;; customize the fonts on different os
(defun load-fonts ()
   "load fonts for different os."
   (cond ((eq system-type 'windows-nt) (cabins/set-monospaced-font "Courier New" "楷体" 13 11.0))
         ((eq system-type 'gnu/linux) (cabins/set-monospaced-font "Courier New" "楷体" 13 11.0))
         ((eq system-type 'darwin) (cabins/set-monospaced-font "Courier New" "STHeiti" 13 16))))

;; load the customized fonts only when in GUI mode
(when (display-graphic-p)
  (load-fonts))

;; reload the fonts & screen layout when in Daemon mode
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
              (load-fonts)
              (cabins/optimize-screen))))

;; disable the bars for emacs 26
(when (< emacs-major-version 27)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; customize the modeline
(require 'init-modeline)

(provide 'init-ui)

;;; init-ui.el ends here
