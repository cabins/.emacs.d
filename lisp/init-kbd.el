;;; init-kbd.el --- Summary
;;; Commentary: (c)Cabins, github.com/cabins/.emacs.d
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
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)

;; alias yes/no to y/p
(defalias 'yes-or-no-p 'y-or-n-p)

;; settings for crux
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-c ^" . crux-top-join-line)
	     ("C-," . crux-find-user-init-file)
         ("C-S-d" . crux-duplicate-current-line-or-region)
         ("C-S-k" . crux-smart-kill-line))) ; We can use C-S-<Backspace> instead.

;; hungry-delete - works exactly like c-hungry-delete-mode
(use-package hungry-delete
  :bind (("C-c <backspace>" . hungry-delete-backward)
         ("C-c <delete>" . hungry-delete-forward)
         ("C-c d" . hungry-delete-forward)))

;; drag-stuff - move lines up/down
(use-package drag-stuff
  :bind (("<M-up>". drag-stuff-up)
         ("<M-down>" . drag-stuff-down)))

;; Settings for jump windows, use M-NUM to switch
(use-package ace-window
  :bind (("C-x o" . 'ace-window)))

(provide 'init-kbd)
;;; init-kbd.el ends here
