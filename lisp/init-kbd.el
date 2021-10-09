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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Global Keybinds Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package crux)
(use-package hungry-delete)
(use-package drag-stuff)
(use-package format-all
  :diminish " Fmt."
  :init (add-hook 'prog-mode-hook 'format-all-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                   Global Key Bindings
;;
;; 当前版本全局按键绑定秉承以下原则：
;; 1. 自定义全局按键尽可能以C-c开头（或绑F5-F9），此为Emacs设计规范预期
;; 2. 记忆方式上，尽可能VSCode相近，因同在用VSCode
;; 3. 不违背Emacs Quirks [http://ergoemacs.org/emacs/keyboard_shortcuts.html]
;; 4. 为方便统一管理，全局按键不分散于use-package中，模式按键仍在use-package中
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Emacs Basic Keys ------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c ,") #'crux-find-user-init-file)	; Open Settings
(global-set-key (kbd "C-c r") 'recentf-open-files) ; Open Recent Files

;; Window Move
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;;; Code Editing ------------------------------
;; Comments（As C-x C-; is for comment-line, keep the postfix）
(global-set-key (kbd "C-c C-;") #'comment-or-uncomment-region)
;; Line Edit
(global-set-key (kbd "M-<down>") #'drag-stuff-down)
(global-set-key (kbd "M-<up>") #'drag-stuff-up)
(global-set-key (kbd "C-c C-d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-a") #'crux-move-beginning-of-line)
;; Delete
(global-set-key (kbd "C-c <backspace>") #'hungry-delete-backward)
(global-set-key (kbd "C-c <delete>") #'hungry-delete-forward)
;; Code Beautify
(global-set-key (kbd "C-o") #'yas-expand)
(global-set-key (kbd "C-c f") #'format-all-buffer)
;; Syntax
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

(provide 'init-kbd)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-kbd.el ends here
