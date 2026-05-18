;; force eshell to push .bashrc
(setq shell-command-switch "-ic")
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(when (>= emacs-major-version 24)
  (require 'package)

  (add-to-list
   'package-archives
   '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   t))

(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-install package)))

(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package vterm
  :load-path  "/Users/dankorytov/emacs-libvterm")



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el")
     (monet :url "https://github.com/stevemolitor/monet")
     (claude-code :url "https://github.com/stevemolitor/claude-code.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((((class color) (background dark)) (:foreground "red" :weight bold)))))

(load-file "~/.emacs.d/inits/generic.el")
(load-file "~/.emacs.d/inits/theme.el")
(load-file "~/.emacs.d/inits/autosave.el")
(load-file "~/.emacs.d/inits/org.el")
(load-file "~/.emacs.d/inits/treesit.el")
(load-file "~/.emacs.d/inits/prog.el")
(load-file "~/.emacs.d/inits/python.el")
(load-file "~/.emacs.d/inits/claude.el")
