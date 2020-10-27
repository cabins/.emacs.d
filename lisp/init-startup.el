;;; init-startup.el --- Works when startup Emacs

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:

;; Settings for system encoding
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  (setq locale-coding-system 'gb18030
        w32-unicode-filenames 'nil
        file-name-coding-system 'gb18030)
  (set-next-selection-coding-system 'utf-16-le)
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le))


;; Settings for backup files
(setq make-backup-files nil
      auto-save-default nil)

;; Adjust garbage collection thresholds during startup
(setq-default frame-title-format '("%f"))

(setq inhibit-startup-screen t)
(setq initial-scratch-message "\n;; Configuration by Cabins <github.com/cabins>.\n;; Enjoy!\n\n")

(provide 'init-startup)
;;; init-startup.el ends here
