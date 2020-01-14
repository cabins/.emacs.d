;;; init-startup.el --- Summary
;;; Commentary:

;;; Code:

;;; Settings for system encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;; Settings for backup files
(setq make-backup-files nil)

(require 'package)
;; (setq package-enable-at-startup t)

;;; Settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))


;; Adjust garbage collection thresholds during startup, and thereafter -from purcell config
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	                (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


(provide 'init-startup)
;;; init-startup.el ends here
