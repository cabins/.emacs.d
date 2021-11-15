;;; init-system.el --- configs for startup -*- lexical-binding: t -*-
;; Author: Cabins
;; Maintainer: Cabins
;; Homepage: https://github.com/cabins

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ====================Common Settings==================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; settings for system coding
(prefer-coding-system 'utf-8)

(setq auto-save-default nil	   ; disable auto save
      auto-window-vscroll nil
      delete-by-moving-to-trash t  ; disable delete directly
      fast-but-imprecise-scrolling t
      frame-title-format "%b"
      help-window-select t
      inhibit-startup-screen t	   ; disable the startup screen splash
      inhibit-default-init t
      ;; initial-scratch-message nil
      inhibit-compacting-font-caches t
      make-backup-files nil             ; disable backup file
      ;; Mouse wheel scroll behavior
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      next-line-add-newlines nil
      read-process-output-max (* 64 1024)
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      visible-bell nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ====================OS Specific==================== ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; macOS
;; move file to trash when delete
(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t))

;; <macOS> Command -> Meta, Option -> Super
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super))

;;; Windows
;; spcial coding settings for Windows
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (setq selection-coding-system 'utf-8))

(provide 'init-system)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-system.el ends here
