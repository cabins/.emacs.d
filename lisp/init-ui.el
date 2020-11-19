;;; init-ui.el --- settings for the Emacs UI

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

    ;;; Code:

;; ;; Settings for UI theme
(use-package almost-mono-themes
  :init (load-theme 'almost-mono-white t))

;; Function to set monofonts
(defun cabins/set-monospaced-font (english chinese e-size c-size)
  "cabins/set-monospaced-font is used for setting monospaced font"
  (set-face-attribute 'default nil
                      :font (font-spec
                             :name english
                             :weight 'normal
                             :slant 'normal
                             :size e-size))
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

(when (display-graphic-p)
  (if *is-windows*
      ;; font setting for Windows platform
      (cabins/set-monospaced-font "Courier New" "Microsoft Yahei Mono" 13 13))
  (if *is-mac*
      ;; font setting for macOS platform
      (cabins/set-monospaced-font "Monaco" "Helvetica" 13 13))
  (if *is-linux*
      ;; font setting for GNU/Linux platform
      (cabins/set-monospaced-font "Monaco" "Helvetica" 13 13))
  )


;; Font settings
(use-package emacs
  :when (display-graphic-p)
  :config
  (set-default 'cursor-type 'bar)
  ;; I prefer the cursor be red color, 'cause it's more obvious.
  (set-face-background 'cursor "#FF0000")
  (setq-default scroll-up-aggressively 0.01
                scroll-down-aggressively 0.01)
  (setq default-frame-alist '((width . 180) (height . 40)))
  (setq redisplay-dont-pause t
        scroll-conservatively most-positive-fixnum
        scroll-margin 1
        scroll-step 1
        scroll-preserve-screen-position 'always)
)

(provide 'init-ui)
;;; init-ui.el ends here
