;;; init-ui.el --- settings for the Emacs UI

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:


;; Settings for UI theme
(use-package gruvbox-theme
  ;; :unless *is-windows*
  :init (load-theme 'gruvbox-dark-soft t))
;; (use-package smart-mode-line-powerline-theme)

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  ;; (if *is-windows*
  ;; (setq sml/theme 'powerline)
  ;; (setq sml/theme 'powerline))
  (sml/setup))

(use-package emacs
  :if (display-graphic-p)
  :config
  ;; Font settings
  (if *is-windows*
      (progn
        (set-face-attribute 'default nil :font "Microsoft Yahei Mono 9")
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset (font-spec :family "Microsoft Yahei Mono" :size 9))))
    (set-face-attribute 'default nil :font "Source Code Pro for Powerline 11"))
  ;; (setq default-frame-alist '((width . 150) (height . 35))
  (set-frame-parameter nil 'fullscreen 'maximized))


(provide 'init-ui)
;;; init-ui.el ends here
