;;; init-elpa --- initialize elpa repository
;;; Commentary:
;;; Code:

(setq package-check-signature nil)

(require 'package)
(setq package-enable-at-startup t)

;;; Settings for package archives
(setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

;;; Package initialize
;; (when (< emacs-major-version 27)
;; (package-initialize))

;;; Initialize the packages, avoiding a re-initialization
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(provide 'init-elpa)
;;; init-elpa.el ends here
