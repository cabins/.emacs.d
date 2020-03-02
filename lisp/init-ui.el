;;; init-ui.el --- settings for the Emacs UI
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; PART 1: UI basic settings
;;; PART 2: Fonts settings
;;; PART 3: Theme settings
;;; PART 4: Frame/Window settings
;;; Code:

(use-package emacs
  :config
  ;; GUI misc
  (setq inhibit-startup-screen t
        initial-buffer-choice nil
        initial-scratch-message (concat ";; Happy hacking, " user-login-name "! Welcome to use the configs from Cabins.\n"
                                        "\n"
                                        ";; Please create issues if you find any bugs. https://github.com/cabins/.emacs.d\n"))
  (when (display-graphic-p)
    (set-frame-parameter nil 'fullscreen 'maximized)
    ;; (setq default-frame-alist '((width . 150) (height . 35))
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    ;; I want display menu bar on macOS when in GUI mode
    (unless *is-mac*
      (menu-bar-mode -1))

    ;; Font settings
    (if *is-windows*
        (progn
          (set-face-attribute 'default nil :font "Microsoft Yahei Mono 9")
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font (frame-parameter nil 'font)
                              charset (font-spec :family "Microsoft Yahei Mono" :size 9)))
          )
      (set-face-attribute 'default nil :font "Source Code Pro for Powerline 11")
      )))

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

(provide 'init-ui)
;;; init-ui.el ends here
