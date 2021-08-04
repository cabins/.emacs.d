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

;; NOTE:
;; The style of use-package is NOT necessary
;; I write code with this style for unification

;; <TAB> show settings
(use-package tab-settings
  :ensure nil
  :init(setq-default tab-width 4
                     indent-tabs-mode nil))

;; C-s/C-r settings
(use-package search-settings
  :ensure nil
  :init (setq-default isearch-lazy-count t
                      lazy-count-prefix-format "%s/%s "))

;; some delete hooks settings
(use-package delete-settings
  :ensure nil
  :hook ((before-save-hook . delete-trailing-whitespace)
         (after-init . delete-selection-mode)))

;; settings for line number
(use-package line-number-settings
  :ensure nil
  :init
  (setq display-line-numbers-type 't) ; relative, visual
  (global-display-line-numbers-mode t))

;; recent files
(use-package recentf-settings
  :ensure nil
  :init
  (setq recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (global-set-key (kbd "C-c f") 'recentf-open-files)
  (recentf-mode 1))

;; Cursor & Current Line
(use-package cursor-line-settings
  :ensure nil
  :init
  ;; blink the cursor
  (blink-cursor-mode 1)
  )

;; [built-in] Toggle hideshow minor mode on
(use-package hideshow-settings
  :ensure nil
  :hook (prog-mode . hs-minor-mode))


(provide 'init-misc)

;;; init-misc.el ends here
