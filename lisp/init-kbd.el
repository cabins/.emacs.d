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
;;                    全局按键包依赖
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package crux)
(use-package hungry-delete)
(use-package drag-stuff)
(use-package format-all :init (add-hook 'prog-mode-hook 'format-all-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                   集中设置全局快捷按键
;;
;; 当前版本全局按键绑定秉承以下原则：
;; 1. 自定义全局按键尽可能以C-c开头（或绑F5-F9），此为Emacs设计规范预期
;; 2. 记忆方式上，尽可能VSCode相近，因同在用VSCode
;; 3. 不违背Emacs Quirks [http://ergoemacs.org/emacs/keyboard_shortcuts.html]
;; 4. 为方便统一管理，全局按键不分散与use-package中，模式按键仍在use-package中
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 按键映射
;; 在macOS上，将Command键映射为Meta，Option映射为Super
(when *is-mac*
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super))

;;; Emacs基本配置 ------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-,") #'crux-find-user-init-file)	; 打开配置文件
(global-set-key (kbd "C-c f") 'recentf-open-files)

;; 窗口移动
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;;; 代码编辑 ------------------------------
;; 注释（由于行注释使用C-x C-;，所以块注释使用相同后缀）
(global-set-key (kbd "C-c C-;") #'comment-or-uncomment-region)
;; 行操作
(global-set-key (kbd "C-c C-<down>") #'drag-stuff-down)	; 向下移动行/块
(global-set-key (kbd "C-c C-<up>") #'drag-stuff-up) ; 向上移动行/块
(global-set-key (kbd "C-c C-d") #'crux-duplicate-current-line-or-region) ;行复制
(global-set-key (kbd "C-a") #'crux-move-beginning-of-line) ; 回到行首
;; 删除操作
(global-set-key (kbd "C-c <backspace>") #'hungry-delete-backward) ; 前空白删除
(global-set-key (kbd "C-c <delete>") #'hungry-delete-forward) ;后空白删除
;; 代码展开与格式化
(global-set-key (kbd "C-o") #'yas-expand)
(global-set-key (kbd "C-c C-f") #'format-all-buffer)
;; 代码检查
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

(provide 'init-kbd)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-kbd.el ends here
