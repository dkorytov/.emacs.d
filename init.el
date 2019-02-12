
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))
(require 'gruvbox)
(load-theme 'gruvbox-dark-hard t)

 (setq column-number-mode t)
(defun prev-window ()
   (interactive)
   (other-window -1))
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "a622aaf6377fe1cd14e4298497b7b2cae2efc9e0ce362dade3a58c16c89e089c" "2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" default)))
 '(org-agenda-files
   (quote
    ("~/org/core.org" "~/org/school.org" "~/org/home.org")))
 '(package-selected-packages (quote (gruvbox-theme flylisp))))
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
  ;; Make a special "per session" backup at the first save of each
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
  "Increase font size by 10 points"
  (interactive)
  (set-face-attribute 'default nil
                      :height
                      (+ (face-attribute 'default :height)
                         15)))

(defun my/zoom-out ()
  "Decrease font size by 10 points"
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
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/school.org" 
                             "~/org/home.org"))
(setq bell-volume 0)
(setq visible-bell 1)
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
(require 'pdf-view)
 
(setq pdf-info-epdfinfo-program "/usr/bin/epdfinfo")
 
(setq pdf-view-midnight-colors `(,(face-attribute 'default :foreground) .
                                  ,(face-attribute 'default :background)))
 
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
 
(add-hook 'pdf-view-mode-hook (lambda ()
                                 (pdf-view-midnight-minor-mode)))
 
(provide 'init-pdfview)

;; Enable flyspell mode by default 
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
