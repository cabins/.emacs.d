;;; init-ui.el --- settings for the Emacs UI

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

(use-package emacs
  :config
  ;; GUI misc
  (setq inhibit-startup-screen t
        initial-buffer-choice nil)
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    ;; Display menu bar on macOS with GUI mode (Global Menu)
    (unless *is-mac*
      (menu-bar-mode -1))

    ;; Font settings
    (if *is-windows*
        (progn
          (set-face-attribute 'default nil :font "Microsoft Yahei Mono 9")
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font (frame-parameter nil 'font)
                              charset (font-spec :family "Microsoft Yahei Mono" :size 9))))
      (set-face-attribute 'default nil :font "Source Code Pro for Powerline 11")
      )
    ;; (setq default-frame-alist '((width . 150) (height . 35))
    (set-frame-parameter nil 'fullscreen 'maximized)))

;; Settings for UI theme
(use-package dracula-theme
  :unless *is-windows*
  :init (load-theme 'dracula t))
;; (use-package smart-mode-line-powerline-theme)
(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  ;; (if *is-windows*
  ;; (setq sml/theme 'powerline)
  ;; (setq sml/theme 'powerline))
  (sml/setup))



(provide 'init-ui)
;;; init-ui.el ends here
