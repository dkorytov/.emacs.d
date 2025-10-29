(use-package projectile
  :ensure t
  )

(use-package yasnippet
  :ensure t
  )

(use-package company
  :ensure t
  :config
  (global-company-mode)
  )

(use-package highlight-indentation
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indentation-mode)
  )

;; diff-hl all buffers
(use-package diff-hl
  :ensure t
  :config 
  (global-diff-hl-mode)
  )

;; (use-package lsp-mode
;;   :enture t
;;   )
;; (use-package lsp
;;   :enture t
;;   )
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package lsp-mode
  :ensure t
  :hook
  (python-mode . lsp)
  :commands lsp
  :config
  (define-key global-map (kbd "M-[") 'lsp-find-definition)
  (define-key global-map (kbd "M-]") 'lsp-find-reference)
  )

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key global-map (kbd "M-/") 'xref-find-references)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 0.2)
  )

(use-package flycheck
  :ensure t
  :config
  (define-key global-map (kbd "C->") 'flycheck-next-error)
  (define-key global-map (kbd "C-<") 'flycheck-previous-error)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . right)
              (reusable-frames . visible)
              ))
