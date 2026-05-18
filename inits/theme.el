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


;; ---------------------------------------------------------------------------
;; Modeline: mood-line (active) — minimal modern modeline.
;;
;; Two prior modelines are kept commented below for easy switch-back:
;;   - doom-modeline + nerd-icons (feature-rich, needs Nerd Fonts)
;;   - smart-mode-line (legacy, slower, what you had originally)
;;
;; Cleanup TODO once mood-line proves itself:
;;   - Delete the commented doom-modeline / nerd-icons / smart-mode-line blocks.
;;   - M-x package-delete RET doom-modeline RET
;;   - M-x package-delete RET nerd-icons RET
;;   - M-x package-delete RET smart-mode-line RET    (and rich-minority if pulled)
;;   - Remove any of these from `package-selected-packages' in init.new.el's
;;     custom-set-variables (if customize wrote them there).
;; ---------------------------------------------------------------------------
;; (use-package mood-line
;;   :ensure t
;;   :config (mood-line-mode))

(use-package nerd-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)                                 ; taller bar
  (doom-modeline-bar-width 6)                               ; wider accent strip
  (doom-modeline-icon t)                                    ; Nerd Font icons in segments
  (doom-modeline-buffer-encoding nil)                       ; hide UTF-8/LF noise
  (doom-modeline-buffer-file-name-style 'truncate-upto-project) ; ~/p/m/project/file.py
  (doom-modeline-vcs-max-length 12)                         ; cap git branch name length
  (doom-modeline-minor-modes nil)                           ; don't list minor mode lighters
  (doom-modeline-project-detection 'project)                ; align with eglot's project.el
  (doom-modeline-check-simple-format t))                    ; "⚠ 2" instead of "0/2/0"

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (sml/setup)
;;   (setq sml/name-width 80)
;;   (setq sml/mode-width 40))

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

;; Use Noto Color Emoji (vector COLRv1) so emoji-bearing lines don't grow
;; taller than surrounding lines. Install with:
;;   brew install --cask font-noto-color-emoji
(when (display-graphic-p)
  (set-fontset-font t 'emoji
                    (font-spec :family "Noto Color Emoji")
                    nil 'prepend)
  (set-fontset-font t 'symbol
                    (font-spec :family "Noto Color Emoji")
                    nil 'append)
  (setq use-default-font-for-symbols nil))
