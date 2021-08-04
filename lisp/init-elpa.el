;;; init-elpa --- initialize elpa repository
;;; Commentary: (c)Cabins, github.com/cabins/.emacs.d
;;; Code:

;;; settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.bfsu.edu.cn/elpa/melpa/")
                         ("gnu" . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
                         ("org" . "http://mirrors.bfsu.edu.cn/elpa/org/"))
      package-check-signature nil
      load-prefer-newer t)

(require 'package)

;;; initialize the packages, avoiding a re-initialization
(unless (bound-and-true-p package--initialized) ;; To avoid warnings on 27
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; settings for use-package package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; configure use-package prior to loading it
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-always-demand nil
        use-package-expand-minimally t))

(eval-when-compile
  (require 'use-package))

(use-package gnu-elpa-keyring-update)
(use-package diminish)
(use-package delight)

(provide 'init-elpa)
;;; init-elpa.el ends here
