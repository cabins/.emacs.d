;;; interactive-funcs.el --- define some useful interactive-funcs
;;; Commentary: (c) Cabins, github.com/cabins/.emacs.d
;;; Code:

(defun cabins/user-login-info ()
  "Print the login user info as init message."
  (let ((prefix ";; Configured by Cabins <github.com/cabins>.\n")
        (os-version (format ";; %20s: %S\n" "Operating System" system-type))
        (user-names (format ";; %20s: %s\n" "Login User" (user-login-name)))
        (machine-name (format ";; %20s: %s\n" "Machine Name" (system-name)))
        (suffix ";; Enjoy!"))
    (concat prefix ";;\n" os-version user-names machine-name ";;\n" suffix)))

(defun cabins/toggle-dark-theme ()
  "Toggle the theme to dark or light."
  (interactive)
  (if custom-enabled-themes
      (disable-theme (car custom-enabled-themes))
    (load-theme 'deeper-blue t)))

(provide 'init-funcs)
;;; interactive-funcs.el ends here
