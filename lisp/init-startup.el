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
(setq-default frame-title-format '("%b"))

(setq inhibit-startup-screen t)
(setq initial-scratch-message (cabins/user-login-info))

;; I don't like the bell ring
(setq ring-bell-function #'ignore
      visible-bell nil)

;; blink the cursor
(blink-cursor-mode 1)

(global-hl-line-mode 1)
;; 高亮当前行，使用浅灰色背景条
(set-face-background hl-line-face "#F2F2F2")
;; 高亮当前行，使用下划线
;; (set-face-underline-p 'highlight t)

(if *is-mac*
    (setq delete-by-moving-to-trash t))

(provide 'init-startup)
;;; init-startup.el ends here
