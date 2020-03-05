;;; core-load-paths.el --- settings for load path

;;; Commentary:

;;; Code:

(defun add-to-load-path (dir)
  "Funtion to add DIR to 'load-path'."
  (add-to-list 'load-path dir))

;; Settings for the lisp config path
(defconst lisp-startup-folder
  (expand-file-name (concat user-emacs-directory "lisp/startup/"))
  "Emacs Lisp folder.")

(defconst lisp-core-folder
  (expand-file-name (concat user-emacs-directory "lisp/core/"))
  "Core folder")

(defconst lisp-languages-folder
  (expand-file-name (concat user-emacs-directory "lisp/languages/"))
  "Lauguage settings folder.")

(defconst lisp-packages-folder
  (expand-file-name (concat user-emacs-directory "lisp/packages/"))
  "Packages settings folder.")

(defconst lisp-ui-folder
  (expand-file-name (concat user-emacs-directory "lisp/ui/"))
  "UI settings folder")

(defconst lisp-misc-folder
  (expand-file-name (concat user-emacs-directory "lisp/misc/"))
  "Misc settings folder")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(mapc 'add-to-load-path
      `(,lisp-startup-folder
	,lisp-core-folder
	,lisp-languages-folder
	,lisp-packages-folder
	,lisp-ui-folder
	,lisp-misc-folder))

(when (file-exists-p custom-file)
  (load-file custom-file))

;;; core-load-paths.el ends here
