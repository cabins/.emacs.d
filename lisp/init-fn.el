;;; init-fn.el --- define some useful interactive functions -*- lexical-binding: t -*-
;; Author: Cabins
;; Maintainer: Cabins

;;; Commentary:
;; the functions are all for debuging.
;; (c) Cabins Kong, 2020-2021

;;; Code:

(defmacro cabins/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun cabins/tmp-reset-elpa ()
  "Reset Elpa temporary.  Useful when emacs-china sync fails."

  (interactive)
  (setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.org/packages/"))))

(provide 'init-fn)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-fn.el ends here
