;;; early-init.el --- Emacs 27 introduces early-init.el, which runs before init.el
;;; Commentary:
;; Runs before package and UI initializetion happens.
;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda ()
			     (setq gc-cons-threshold 800000)))

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; System default coding
(set-language-environment 'utf-8)

;; Load theme early to avoid the blinking
(if (>= emacs-major-version 28)
    (load-theme 'modus-operandi t)
  (load-theme 'leuven t))

;; Cleaner GUI
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; early-init.el ends here
