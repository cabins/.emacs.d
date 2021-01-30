;;; init-vue.el --- http mode settings

;;; Commentary:

;;; Code:

(use-package vue-mode
  ;; disable the ugly background color
  ;; https://github.com/AdamNiederer/vue-mode#how-do-i-disable-that-ugly-background-color
  :config (set-face-background 'mmm-default-submode-face nil))

(provide 'init-vue)

;;; init-vue.el ends here
