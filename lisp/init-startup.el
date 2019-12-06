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


;;; Settings for delete multi line spaces
(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-startup)
;;; init-startup.el ends here.
