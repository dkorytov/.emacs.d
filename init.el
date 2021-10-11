
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   t)
  (add-to-list
   'package-archives
   '("milkbox" . "http://melpa.milkbox.net/packages/") ; couldn't download sphinx-doc
   t))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package flycheck :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package xterm-color :ensure t)
(use-package jedi :ensure t)
(use-package smart-mode-line :ensure t)
(use-package sphinx-doc :ensure t)
(use-package elpy :ensure t)
(use-package projectile :ensure t)
(use-package helm :ensure t)
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(elpy-enable)

;; Hook for blacken
(add-hook 'python-mode-hook 'python-black-on-save-mode)
;; (setq python-black-extra-args)
;; (setq python-black--config-file ".black.toml")

;; Sphinx docs for python
(add-hook 'python-mode-hook (lambda ()
			      (require 'sphinx-doc)
			      (sphinx-doc-mode t)))
;; Smartmodeline package
(setq sml/no-confirm-load-theme t) ;; removes "loading a theme can run Lisp code"
(sml/setup)
(add-to-list 'sml/replacer-regexp-list '("^~/work/scripts/" ":scripts:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/work/scripts2/" ":scripts2:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/work/analytics/" ":analytics:") t)
(add-to-list 'sml/replacer-regexp-list '("^~/work/oakheart/" ":OkHrt:") t)
(setq sml/name-width 80)
(setq sml/mode-width 40)


(add-hook 'after-init-hook #'global-flycheck-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "a622aaf6377fe1cd14e4298497b7b2cae2efc9e0ce362dade3a58c16c89e089c" "2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" default)))
 '(flycheck-python-flake8-executable "flake8")
 '(flycheck-python-mypy-executable "mypy")
 '(flycheck-python-pycompile-executable "python")
 '(org-agenda-files (quote ("~/org/core.org" "~/org/school.org")))
 '(package-selected-packages
   (quote
    (python-black helm markdown-mode flycheck-projectile magit elpy projectile exec-path-from-shell xterm-color use-package sphinx-doc smart-mode-line flycheck jedi gruvbox-theme flylisp))))

(setq flycheck-check-syntax-automatically '(mode-enabled save))
(setq-default flycheck-disabled-checkers '(python-pylint)) ;; prevent pylint checker from running
(add-to-list 'flycheck-python-mypy-config '".mypy.ini")

(defun setup-flycheck-python-project-path ()
  (interactive)
  (let ((root (ignore-errors (projectile-project-root))))
    (when root
      (add-to-list
       (make-variable-buffer-local 'flycheck-python-import-path)
       root))))

(add-hook 'python-mode-hook 'setup-flycheck-python-project-path)

;; Gruvbox theme
(require 'gruvbox)
(load-theme 'gruvbox-dark-hard t)

;; eshell colors to display properly
(add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "xterm-256color")))
(add-hook 'eshell-before-prompt-hook (setq xterm-color-preserve-properties t))

(setq column-number-mode t)
(defun prev-window ()
  "Return to the previous window."
   (interactive)
   (other-window -1))

;; Quickly setup git
(defun git-config-init ()
  "Setup git on a clean machine."
  (interactive)
  (shell-command "git config --global user.email \"korytov@energyhub.net\"")
  (shell-command "git config --global user.name \"Dan Korytov\"")
  (shell-command "git config --global push.default simple")
  (shell-command "git config --global core.editor emacs")
  (shell-command "git config --global core.pager 'cat'")
  (shell-command "git config --global alias.ll '!git log --pretty=format:'%C(auto)%h%d (%cr) %s' --graph --decorate -n'")
  (shell-command "git config --global alias.sl '!git status && git ll 10 && echo ""'")
  ;; (shell-command "git config --global alias.bl '!git reflog show --pretty=format:'%gs ~ %gd' --date=relative | grep 'checkout:' | grep -oE '[^ ]+ ~ .*' | awk -F~ '!seen[$1]++' | head -n 10 | sed 's/~ HEAD@{/(/' | sed 's/}/)/''")
  (message "git config is setup!"))

(defun flycheck-pip-install()
  "Quickly installs pip packages for flycheck"
  (interactive)
  (shell-command "pip install -r ~/.emacs.d/requirements.txt"))

(define-key global-map (kbd "C-x p") 'prev-window)
(define-key global-map (kbd "<f11>") 'toggle-full-screen)
(define-key global-map (kbd "M-<down>") 'scroll-up-line)
(define-key global-map (kbd "M-<up>") 'scroll-down-line)
(define-key global-map (kbd "M-n") 'scroll-up-line)
(define-key global-map (kbd "M-p") 'scroll-down-line)
(define-key global-map (kbd "M-:") 'comment-box)

(setq hs-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c @ h")   'hs-hide-block)
        (define-key map (kbd "C-c @ s")   'hs-show-block)
        (define-key map (kbd "C-c @ H")	  'hs-hide-all)
        (define-key map (kbd "C-c @ S")	  'hs-show-all)
        (define-key map (kbd "C-c @ l")   'hs-hide-level)
        (define-key map (kbd "C-c @ @")   'hs-toggle-hiding)
        (define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
map))
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'linum-mode)
(defadvice goto-line (after expand-after-goto-line
			    activate compile)
  "hideshow-expand affected block when using goto-line in a
collapsed buffer"
  (save-excursion
    (hs-show-block)))
;; (setq default-frame-alist
;;       (append default-frame-alist
;;        '((foreground-color . "#E0DFDB")
;;  (background-color . "black")
;;  (cursor-color . "gray")
;;  )))
;; (set-face-foreground 'font-lock-comment-face "red")
(setq inhibit-startup-message t)
(menu-bar-mode -99)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))
(set-default 'truncate-lines t)
(setq-default fill-column 120)

;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
; '(default ((t (:inherit nil :stipple nil :background "#000302" :foreground "#E0DFDB" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 127 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((((class color) (background dark)) (:foreground "red" :weight bold)))))

(require 'generic-x) ;; we need this

(define-generic-mode
  'param-mode                         ;; name of the mode to create
  '("#")                           ;; comments start with '!!'
  '("false" "true", "True", "False");; some keywords
  '(("$" . 'font-lock-operator)     ;; '=' is an operator
    ("{}" . 'font-lock-builtin))     ;; ';' is a a built-in
  '("\\.param$")                      ;; files for which to activate this mode
   nil                              ;; other functions to call
  "A mode for param files"            ;; doc string for this mode
)


;;from: http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;;Auto backups setting up
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them


;; Next, also backup versioned files, which Emacs does not do by default (you don't commit on every save, right?)
(setq vc-make-backup-files t)

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  "Make a special 'per session' backup at the first save of each."
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;;a clear function for eshell
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;;only kills/copies text if the area is active
(setq mark-even-if-inactive nil)

;;start emacs in eshell by default or at the command line
;;specified file
;;(add-hook 'emacs-startup-hook (lambda () (eshell)))
(defun my/zoom-in ()
  "Increase font size by 10 points."
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         15)))

(defun my/zoom-out ()
  "Decrease font size by 10 points."
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (- (face-attribute 'default :height)
                         15)))

;; change font size, interactively
(global-set-key (kbd "C->") 'my/zoom-in)
(global-set-key (kbd "C-<") 'my/zoom-out)

(require 'org)
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE" "|" "DFRD")))
(setq org-todo-keyword-faces
      `(("DONE" . org-done) ("DOING" . "orange") ("DFRD" . "#696FCD")))
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/school.org"
                             "~/org/home.org"))
(setq org-log-done 'time)

(setq bell-volume 0)
(setq visible-bell 1)

;; set fill-paragraph to interact with fill paragraph correctly
(setq python-fill-docstring-style 'django)
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	;; This would override `fill-column' if it's an integer.
	(emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)



;; PDF viewer
;; (require 'pdf-view)
 
;; (setq pdf-info-epdfinfo-program "/usr/bin/epdfinfo")
 
;; (setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
;;                                   ,(face-attribute 'default :background)))
 
;; (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
 
;; (add-hook 'pdf-view-mode-hook (lambda ()
;;                                  (pdf-view-midnight-minor-mode)))
 
;; (provide 'init-pdfview)

;; Enable flyspell mode by default
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(put 'downcase-region 'disabled nil)
(setq confirm-kill-emacs 'y-or-n-p)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;; (add-hook 'eshell-preoutput-filter-functions
;;            'ansi-color-filter-apply)
(add-hook 'eshell-preoutput-filter-functions
           'ansi-color-apply)

(setq frame-title-format
          '("" (:eval
             (if buffer-file-name
                 (abbreviate-file-name buffer-file-name)
               "%b"))))


;; Make 'M-shell' appear in current buffer
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

;; add easy todo line
(defun td ()
    "Writes out a todo line with the current date."
    (interactive)
    (insert "# TODO Dan K [")
    (insert (format-time-string "%Y-%m-%d"))
    (insert "]: "))

(defun nt ()
    "Writes out a todo line with the current date."
    (interactive)
    (insert "# Note Dan K [")
    (insert (format-time-string "%Y-%m-%d"))
    (insert "]: "))

(defun rb ()
  "Rerverts buffer."
  (interactive)
  (revert-buffer))

(defun rby ()
  "Rerverts buffer with auto yes."
  (interactive)
  (revert-buffer nil `true))

;; set completion list sorting to be up/down instead left/right.
(setq completions-format `vertical)

;; set a line length limit for grep when it hits large files
(grep-compute-defaults)
(grep-apply-setting 'grep-find-template
  (concat grep-find-template " | cut -c 1-2000"))

;; Add git branch to eshell prompt
;; ref: https://superuser.com/questions/890937/how-to-show-git-branch-in-emacs-shell
(defun git-prompt-branch-name ()
    "Get current git branch name."
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))

(defun dkorytov:eshell-prompt ()
  "Prompt for eshell with git branch."
  (let ((branch-name (git-prompt-branch-name)))
    (concat
     (if branch-name (format "\n[%s]\n" branch-name) "\n")
     (abbreviate-file-name (eshell/pwd)) " $ "
     )))

(setq eshell-prompt-function #'dkorytov:eshell-prompt
      eshell-prompt-regexp ".*$+ ")

;; remove gui pop up windows
(setq use-dialog-box nil)

(provide `init)
(put 'upcase-region 'disabled nil)
