;;; init-startup.el --- Works when startup Emacs
;;; Commentary:(c) Cabins, github.com/cabins/.emacs.d
;;; Code:

;; Settings for system encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-next-selection-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))

;; Settings for backup files
(setq make-backup-files nil
      auto-save-default nil)

(setq-default frame-title-format '("%b"))
(setq inhibit-startup-screen t)
(setq initial-scratch-message (cabins/user-login-info))

;; I don't like the bell ring
(setq ring-bell-function #'ignore
      visible-bell nil)

(if *is-mac*
    (setq delete-by-moving-to-trash t))

(provide 'init-startup)
;;; init-startup.el ends here
