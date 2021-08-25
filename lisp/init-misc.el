;;; init-misc.el --- misc configs -*- lexical-binding: t -*-

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PLEASE NOTE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE STYLE OF USE-PACKAGE IS NOT NECESSARY
;; I WRITE CODE WITH THIS STYLE FOR UNIFICATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [built-in] Cursor & Current Line
(use-package cursor-line
  :ensure nil
  :init
  (setq-default cursor-type 'bar)
  (blink-cursor-mode 1)
  )

;; [built-in] some delete hooks settings
(use-package delete
  :ensure nil
  :hook ((before-save-hook . delete-trailing-whitespace)
         (after-init . delete-selection-mode)))

;; [built-in] Toggle hideshow minor mode on
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

;; [built-in] C-s/C-r settings
(use-package isearch
  :ensure nil
  :init (setq-default isearch-lazy-count t
                      lazy-count-prefix-format "%s/%s "))

;; [built-in] settings for line number
(use-package line-number
  :ensure nil
  :init
  ;; (setq display-line-numbers-type 't)   ; t(default),relative,visual
  (global-display-line-numbers-mode t))

;; [built-in] recent files
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (global-set-key (kbd "C-c f") 'recentf-open-files)
  (recentf-mode 1))

(provide 'init-misc)

;;; init-misc.el ends here
