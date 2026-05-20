;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(when (>= emacs-major-version 24)
  (require 'package)

  ;; Stable MELPA — only released versions of packages.
  (add-to-list
   'package-archives
   '("melpa-stable" . "https://stable.melpa.org/packages/")
   t)

  ;; Regular MELPA — bleeding-edge snapshots. Required for packages that don't
  ;; cut stable releases (e.g. flymake-ruff). When a package exists on both,
  ;; package.el picks based on `package-archive-priorities' (default: melpa-stable
  ;; wins because it's listed first in version comparisons).
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t))

(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-install package)))

(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package vterm
  :load-path  "/Users/dankorytov/emacs-libvterm")

;; Pull PATH and other env vars from the user's login shell into Emacs.
;; Without this, GUI Emacs on macOS launches with a barebones environment
;; (no Homebrew tools, no python venvs, etc.). This runs the shell ONCE at
;; startup — unlike `shell-command-switch "-ic"' which would re-source it
;; for every subprocess Emacs spawns.
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))



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
(load-file "~/.emacs.d/inits/markdown.el")
(load-file "~/.emacs.d/inits/claude.el")
