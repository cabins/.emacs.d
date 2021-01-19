;;; init-modeline.el --- Summary
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

;; settings for modeline when active
(set-face-attribute 'mode-line nil
                    :background "#e4e5e7"
                    :foreground "black"
                    :box '(:color "#e4e5e7" :line-width 0)
                    :overline nil
                    :underline nil)

;; settings for modeline when inactive
;; make the modeline not annoying as possible
(set-face-attribute 'mode-line-inactive nil
                    :background "#f2f3f4"
                    :foreground "black"
                    :box '(:color "#f2f3f4" :line-width 0)
                    :overline nil
                    :underline nil)

(provide 'init-modeline)

;;; init-modeline.el ends here
