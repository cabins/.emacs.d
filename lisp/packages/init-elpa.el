;;; init-elpa --- initialize elpa repository

;;; Commentary:
;;; (c)Cabins, github.com/cabins/.emacs.d

;;; Code:

;;; Settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(setq package-check-signature nil)

(require 'package)

;;; Initialize the packages, avoiding a re-initialization
(unless (bound-and-true-p package--initialized) ;; To avoid warnings on 27
  ;; (when (version< emacs-version "27.0")
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Settings for use-package package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package prior to loading it
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
	use-package-always-defer t
	use-package-always-demand nil
	use-package-expand-minimally t
	use-package-verbose t))

(provide 'init-elpa)
;;; init-elpa.el ends here
