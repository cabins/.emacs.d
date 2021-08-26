;;; init-kbd.el --- configs for key bind -*- lexical-binding: t -*-

;; Author: Cabins
;; Maintainer: Cabins
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins
;; Keywords:

;;; Commentary:
;; (c) Cabins Kong, 2020-2021

;;; Code:

;; 在macOS上，将Command键映射为Meta，Option映射为Super
(when *is-mac*
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super))

;; 在Windows上，将App/Menu键映射为Hyper
(when *is-windows*
  (setq w32-apps-modifier 'hyper))

;; global key-binding settings for comment (jetbrains style)
(global-set-key (kbd "C-/") 'comment-line)
;; Acturally this is conflict with emacs quirks
;; Emacs quirks refs: http://ergoemacs.org/emacs/keyboard_shortcuts.html
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)

;; alias yes/no to y/p
(defalias 'yes-or-no-p 'y-or-n-p)

;; settings for crux
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-," . crux-find-user-init-file)
         ("C-S-d" . crux-duplicate-current-line-or-region)))


;; hungry-delete - works exactly like c-hungry-delete-mode
(use-package hungry-delete
  :bind (("C-c <backspace>" . hungry-delete-backward)
         ("C-c <delete>" . hungry-delete-forward)
         ("C-c d" . hungry-delete-forward)))

;; drag-stuff - move lines up/down
;; NOT work in Elisp mode (conflict with paredit)
(use-package drag-stuff
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))


(provide 'init-kbd)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-kbd.el ends here
