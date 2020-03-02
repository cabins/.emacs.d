;;; init-startup.el --- Works when startup Emacs
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
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

(provide 'init-startup)
;;; init-startup.el ends here
