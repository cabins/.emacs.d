;;; init-ui.el --- Summary
;;; Commentary:
;;; Code:

;; Initialize the GUI size & fonts
;; Settings for GUI mode
(when (display-graphic-p)
  (if *is-windows*
      (progn
        (set-face-attribute 'default nil :font "Microsoft Yahei Mono 9")
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset (font-spec :family "Microsoft Yahei Mono" :size 9)))
        )
    (set-face-attribute 'default nil :font "Source Code Pro for Powerline 11")
    ))

;; Settings for UI theme
(use-package spacemacs-theme
  :unless *is-windows*
  :init (load-theme 'spacemacs-dark t))
(use-package smart-mode-line-powerline-theme)
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (if *is-windows*
      (setq sml/theme 'light-powerline)
    (setq sml/theme 'powerline))
  (sml/setup))

;; Setting initial-scratch-message
(setq initial-scratch-message
      (concat ";; Happy hacking, " user-login-name "! Welcome to use the configs from Cabins.\n"
              "\n"
              ";; Please create issues if you find any bugs.\n"
              ";; https://github.com/cabins/.emacs.d\n"
              "\n"
              ";; Emacs loves you!"))

;; Resize the initial window frame
;; I Like maximize the frame window, Comment it if you do NOT like this.
(when (display-graphic-p)
  (set-frame-parameter nil 'fullscreen 'maximized)
  ;; (setq default-frame-alist '((width . 150) (height . 35)))
  )

(provide 'init-ui)
;;; init-ui.el ends here
