;;; init-builtin.el --- initialize the builtin plugins -*- lexical-binding: t -*-

;; Author: Cabins 
;; Maintainer: Cabins 
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/cabins 
;; Keywords: 


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; (c) Cabins Kong, 2020-2021 

;;; Code:

;; settings for org mode and load config from org file
(use-package org
  ;; :init (setq org-startup-indented t)
  :config
  (setq org-startup-indented t
	    org-todo-keywords '((sequence "TODO" "DOING" "DONE"))
	    org-todo-keyword-faces '(("DOING" . "blue"))))

;; Use ido instead of ivy & counsel & swiper
;; They are great! But I want cleaner.
(use-package ido
  :defer nil
  :init
  (setq ido-enable-flex-matching t
        ido-everywhere t
        ido-use-filename-at-point t)
  (ido-mode t)
  (if (< emacs-major-version 27)
      (icomplete-mode t)
    (fido-mode t)))

;; Enable flymake on default, which is built in emacs
(use-package flymake
  :ensure nil
  :diminish (flymake " Flym.")
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(use-package paren
  :config (show-paren-mode 1))

;; Settings for electric-pair
(use-package electric
  :hook ((after-init . electric-indent-mode)
	     (prog-mode . electric-pair-mode)))

;; Abbrev mode
(use-package abbrev-mode
  :ensure nil
  :init (setq-default abbrev-mode t))

;; display 'lambda' as 'λ' (just for fun)
(use-package prettify-symbols-mode
  :ensure nil
  :init (global-prettify-symbols-mode 1))

;; settings for windmove, replace the ace-window plugin
(use-package windmove
  :ensure nil
  :bind (("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)))


(provide 'init-builtin)

;;; init-builtin.el ends here
