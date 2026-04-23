;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gruvbox-theme
  :ensure t
  :config 
  (load-theme 'gruvbox-dark-hard t)
)

(defun light-mode()
  "Set gruv box theme to light mode."
  (interactive)
  (load-theme 'gruvbox-light-hard t))

(defun dark-mode()
  "Set gruv box theme to dark mode."
  (interactive)
  (load-theme 'gruvbox-dark-hard t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((((class color) (background dark)) (:foreground "red" :weight bold)))))


(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (setq sml/name-width 80)
  (setq sml/mode-width 40)
)

(use-package auto-dim-other-buffers
    :ensure t
    :config
    (setq auto-dim-other-buffers-dim-on-focus-out t)
    (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
    ;; ensure org-indent face is properly hidden
    (when (boundp 'auto-dim-other-buffers-affected-faces)
      (push '(org-indent . auto-dim-other-buffers-hide-face)
            auto-dim-other-buffers-affected-faces))
    (auto-dim-other-buffers-mode t))

;; (use-package selected-window-accent-mode
;;   :config (selected-window-accent-mode 1)
;;   :custom
;;   (selected-window-accent-fringe-thickness 10)
;;   (selected-window-accent-custom-color nil)
;;   (selected-window-accent-mode-style 'subtle))
