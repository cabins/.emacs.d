;;; init-startup.el --- Works when startup Emacs

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

;; Settings for system encoding
(prefer-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Settings for backup files
(setq make-backup-files nil
      auto-save-default nil)

;; Adjust garbage collection thresholds during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(setq-default frame-title-format '("%f"))


(provide 'init-startup)
;;; init-startup.el ends here
