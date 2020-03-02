;;; init-elpa --- initialize elpa repository

;;; Commentary:
;;; (c)Cabins, github.com/cabins/.emacs.d

;;; Code:

(setq package-check-signature nil)
(require 'package)

;;; Settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;;; Initialize the packages, avoiding a re-initialization
(unless (bound-and-true-p package--initialized) ;; To avoid warnings on 27
  (setq package-enable-at-startup nil) ;; To prevent initializing twice
  (when (version< emacs-version "27.0")
    (package-initialize)))

(unless package-archive-contents
  (package-refresh-contents))

(provide 'init-elpa)
;;; init-elpa.el ends here
