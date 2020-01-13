;;; init-ui.el --- Summary
;;; Commentary:

;;; Code:

;;; Settings for GUI mode
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      ;;; Init GUI size
      ;; (setq default-frame-alist
	    ;; '((width . 150) (height . 35))
      ;; )

      ;; Init GUI size as maximized mode
      (add-to-list 'default-frame-alist '(fullscreen . maximized))

       ;;; Setting English Font
      (set-face-attribute 'default nil :font "Ubuntu Mono 11")

      ;; Setting Chinese Font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font (frame-parameter nil 'font)
			  charset
			  (font-spec :family "Microsoft Yahei" :size 12)))
      )
  )

;;; Settings for startup
(setq inhibit-startup-screen t)

;;; Settings for line number
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;;; Settings for electric-pair
(add-hook 'prog-mode-hook 'electric-pair-mode)

;;; Settings for UI theme
(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

(provide 'init-ui)
;;; init-ui.el ends here
