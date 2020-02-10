;;; init-startup.el --- Works when startup Emacs
;;; Commentary:
;;; Code:

;; Settings for system encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Settings for backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Adjust garbage collection thresholds during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Settings for GUI misc
(setq inhibit-startup-screen t
      initial-buffer-choice nil)

(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; I want display menu bar on macOS when in GUI mode
(unless (and (display-graphic-p) *is-mac*)
  (menu-bar-mode -1))

;; Settings for line number
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Settings for electric-pair
(add-hook 'prog-mode-hook 'electric-pair-mode)

(provide 'init-startup)
;;; init-startup.el ends here
