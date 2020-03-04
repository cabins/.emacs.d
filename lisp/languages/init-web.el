;;; init-web.el --- web settings

;;; Commentary:

;;; Code:


(use-package web-mode
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-current-element-highlight t
        web-mode-enable-css-colorization t)
  (use-package company-web
    :config
    (add-to-list 'company-backends 'company-web-html)
    (add-to-list 'company-backends 'company-css))
  )

(use-package emmet-mode
  :hook (web-mode css-mode))

(provide 'init-web)
;;; init-web.el ends here
