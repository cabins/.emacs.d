;;; init-elpa --- initialize elpa repository

;;; Commentary:
;;; (c)Cabins, github.com/cabins/.emacs.d

;;; Code:

;;; Settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                         ("gnu" . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ("org" . "http://mirrors.cloud.tencent.com/elpa/org/")))

;; (setq package-check-signature nil)

(require 'package)

;;; Initialize the packages, avoiding a re-initialization
(unless (bound-and-true-p package--initialized) ;; To avoid warnings on 27
  ;; (when (version< emacs-version "27.0")
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Settings for use-package package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package prior to loading it
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-always-demand nil
        use-package-expand-minimally t
        use-package-verbose t))
(setq load-prefer-newer t)

(eval-when-compile
  (require 'use-package))

(use-package gnu-elpa-keyring-update)
(use-package diminish)
(use-package delight)

(provide 'init-elpa)
;;; init-elpa.el ends here
