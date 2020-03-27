;;; init-ui.el --- settings for the Emacs UI

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:


;; Settings for UI theme
(use-package gruvbox-theme
  :unless *is-windows*
  :when (display-graphic-p)
  :init (load-theme 'gruvbox-dark-soft t))
;; (use-package smart-mode-line-powerline-theme)

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t
	    sml/theme 'respectful)
  ;; (if *is-windows*
  ;; (setq sml/theme 'powerline
  ;; sml/theme 'powerline))
  (sml/setup))

;; Font settings
(use-package emacs
  :config
  (set-default 'cursor-type 'bar)
  (setq default-frame-alist '((width . 150) (height . 35)))
  ;; (set-frame-parameter nil 'fullscreen 'maximized)
  (if *is-windows*
      (progn
        (set-face-attribute 'default nil :font "Microsoft Yahei Mono 9")
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset (font-spec :family "华文楷体" :size 10.5))))
    (set-face-attribute 'default nil :font "Source Code Pro for Powerline 11")))

(provide 'init-ui)
;;; init-ui.el ends here
