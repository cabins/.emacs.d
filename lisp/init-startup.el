;;; init-startup.el --- configs for startup -*- lexical-binding: t -*-
;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; settings for system encoding
(set-language-environment "UTF-8")
(unless *is-windows*
  (setq selection-coding-system 'utf-8))

(setq make-backup-files nil             ; disable backup file
      auto-save-default nil
      inhibit-startup-screen t          ; disable the startup screen splash
      inhibit-default-init t
      visible-bell nil
      inhibit-compacting-font-caches t
      read-process-output-max (* 64 1024))

;; macOS: move file to trash when delete
(when *is-mac* (setq delete-by-moving-to-trash t))

(provide 'init-startup)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-startup.el ends here
