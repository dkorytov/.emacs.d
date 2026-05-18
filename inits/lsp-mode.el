;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-mode + lsp-ui + flycheck (NOT loaded by default)
;;
;; This is the previous LSP/diagnostic stack, preserved for reference and
;; quick switch-back. It is intentionally not listed in `init.new.el', so
;; nothing here runs at startup.
;;
;; The active stack is now eglot (in `prog.el') + flymake.
;;
;; To switch back to lsp-mode/flycheck:
;;   1. Comment out the eglot `use-package' block and the flymake bindings
;;      in `prog.el'.
;;   2. Add `(load-file "~/.emacs.d/inits/lsp-mode.el")' to `init.new.el'.
;;   3. Restart Emacs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :ensure t
  :hook
  (python-base-mode . lsp)
  :commands lsp
  :config
  (define-key global-map (kbd "M-[") 'lsp-find-definition)
  (define-key global-map (kbd "M-]") 'lsp-find-reference))

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key global-map (kbd "M-/") 'xref-find-references)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 0.2))


;;----------------------------------------------------------------------------
;; flycheck — diagnostic surface used by lsp-mode (eglot uses flymake instead).
;; Moved here from prog.el / python.el / init.new.el during the eglot switch.
;;----------------------------------------------------------------------------

(use-package flycheck
  :ensure t
  :config
  (define-key global-map (kbd "C->") 'flycheck-next-error)
  (define-key global-map (kbd "C-<") 'flycheck-previous-error)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; Open the *Flycheck errors* buffer in a right-side window.
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . right)
              (reusable-frames . visible)))

;; Make pylint resolve intra-project imports by adding the projectile root to
;; flycheck-python-import-path.
(defun setup-flycheck-python-project-path ()
  (interactive)
  (let ((root (ignore-errors (projectile-project-root))))
    (when root
      (add-to-list
       (make-variable-buffer-local 'flycheck-python-import-path)
       root))))
(add-hook 'python-base-mode-hook 'setup-flycheck-python-project-path)

;; Force pylint as the flycheck checker for python buffers, then chain pyright.
(add-hook 'python-base-mode-hook
          (lambda () (setq flycheck-checker 'python-pylint)))
(flycheck-add-next-checker 'python-pylint 'python-pyright)
