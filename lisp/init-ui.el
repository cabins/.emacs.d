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

(defun cabins/1st-available-font (font-list)
  "Get the first available font of FONT-LIST."
  (catch 'return
    (dolist (font font-list)
      (when (find-font (font-spec :name font))
        (throw 'return font)))))

;; function to set monofonts
(defun cabins/set-fonts ()
  "Set the fonts, inspired by URL `http://ergoemacs.org/emacs/emacs_list_and_set_font.html'"
  (let ((default-fonts '("Courier New" "Ubuntu Mono" "Monaco" "Source Code Pro" "Menlo" "Consolas"))
        (emoji-fonts '("Apple Color Emoji" "Symbola" "Symbol"))
        (chinese-fonts '("楷体" "Microsoft Yahei" "Heiti SC" "WenQuanYi Micro Hei")))
    ;; set the default font
    (set-face-attribute 'default nil
                        :font (font-spec :name (cabins/1st-available-font default-fonts)
                                         :size (cond (*is-windows* 13)
                                                     (*is-mac* 13)
                                                     (t 12))))
    ;; set the emoji font
    (set-fontset-font t 'unicode (cabins/1st-available-font emoji-fonts) nil 'prepend)
    ;; set Chinese font
    (set-fontset-font t '(#x4e00 . #x9fff)
                      (font-spec :name (cabins/1st-available-font chinese-fonts)
                                 :size (cond
                                        (*is-windows* 11.0)
                                        (*is-mac* 16)
                                        (t 10.5))))))

;; 尝试解决字体卡顿问题
(setq inhibit-compacting-font-caches t)

;; load the customized fonts only when in GUI mode
(when (display-graphic-p)
  (cabins/set-fonts))

;; reload the fonts & screen layout when in Daemon mode
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (when (window-system frame)
              (cabins/set-fonts)
              (cabins/optimize-screen))))

;; disable the bars for emacs 26
(when (< emacs-major-version 27)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; (load-theme 'leuven t)
;; customize the modeline
(if (display-graphic-p)
    (require 'init-modeline)
  )


(provide 'init-ui)

;;; init-ui.el ends here
