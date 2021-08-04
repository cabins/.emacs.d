;;; init-kbd.el --- configs for key bind -*- lexical-binding: t -*-

;; Author: Cabins 
;; Maintainer: Cabins 
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins 
;; Keywords: 


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; (c) Cabins Kong, 2020-2021 

;;; Code:

;; 在macOS上，将Command键映射为Meta，Option映射为Super
(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; 在Windows上，将App/Menu键映射为Hyper
(when *is-windows*
  (setq w32-apps-modifier 'hyper))

;; global key-binding settings for comment (jetbrains style)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-?") 'comment-or-uncomment-region) ; Acturally this is conflict with emacs quirks
;; Emacs quirks refs: http://ergoemacs.org/emacs/keyboard_shortcuts.html

;; alias yes/no to y/p
(defalias 'yes-or-no-p 'y-or-n-p)

;; settings for crux
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-," . crux-find-user-init-file)
         ("C-S-d" . crux-duplicate-current-line-or-region)
         ("C-S-k" . crux-smart-kill-line))) ; We can use C-S-<Backspace> instead.


;; hungry-delete - works exactly like c-hungry-delete-mode
(use-package hungry-delete
  :bind (("C-c <backspace>" . hungry-delete-backward)
         ("C-c <delete>" . hungry-delete-forward)
         ("C-c d" . hungry-delete-forward)))

;; drag-stuff - move lines up/down
(use-package drag-stuff)


(provide 'init-kbd)

;;; init-kbd.el ends here
