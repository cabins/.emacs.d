;;; init-ui.el --- settings for the Emacs UI

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

;; Settings for UI theme
(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
	    sml/theme 'respectful)
  (sml/setup))

;; Font settings
(use-package emacs
  :when (display-graphic-p)
  :config
  (set-default 'cursor-type 'bar)
  (setq default-frame-alist '((width . 180) (height . 40)))
  ;; (set-frame-parameter nil 'fullscreen 'maximized)
  ;; Set fonts global

  (when *is-windows*
    (setq face-font-rescale-alist '(("Microsoft Yahei Mono" . 1)))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "Microsoft Yahei Mono" :size 10.5))))
  (when *is-mac*
    (set-face-attribute 'default nil :font "Monaco 11")
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "华文细黑" :size 14.5)))))

(provide 'init-ui)
;;; init-ui.el ends here
