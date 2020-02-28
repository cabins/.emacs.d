;;; init-ui.el --- Summary
;;; Commentary:
;;; Code:

;; Initialize the GUI size & fonts
;; Settings for GUI mode
(use-package smart-mode-line-powerline-theme)

(when (or *is-mac* *is-linux*)
  (progn
    ;; Font settings
    (when (display-graphic-p)
      (set-face-attribute 'default nil :font "Source Code Pro for Powerline 11")
      )
    ;; Settings for UI theme
    (use-package spacemacs-theme
      :init (load-theme 'spacemacs-dark t))
    (use-package smart-mode-line
      :init
      (setq sml/no-confirm-load-theme t
            sml/theme 'powerline)
      (sml/setup))))

(when *is-windows*
  (progn
    ;; Font settings
    (when (display-graphic-p)
      (set-face-attribute 'default nil :font "Microsoft Yahei Mono 9")
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family "Microsoft Yahei Mono" :size 9)))
      )
    (use-package smart-mode-line
      :init (setq sml/no-confirm-load-theme t
                  sml/theme 'light-powerline)
      (sml/setup))))

(use-package emacs
  :config
  (display-time-mode 1))

(provide 'init-ui)
;;; init-ui.el ends here
