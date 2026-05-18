(use-package projectile
  :ensure t
  :custom
  ;; Cache the resolved project root for 10 minutes. Without this, each call
  ;; to `projectile-project-root' walks `file-truename' chains (32ms profiled
  ;; per python buffer open). Refresh happens automatically after the timeout.
  (projectile-project-root-cache-timeout 600))

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

;; eglot — built-in LSP client (Emacs 29+). No `:ensure' since it ships with
;; Emacs. eglot wires into the standard Emacs facilities: xref for navigation,
;; flymake for diagnostics, eldoc for hover, completion-at-point (which
;; company picks up) for completion.
;;
;; LSP server selection is automatic via `eglot-server-programs' — eglot will
;; find pyright/basedpyright/pylsp on PATH for python-base-mode buffers.
;;
;; The previous lsp-mode config is preserved (but not loaded) in
;; `inits/lsp-mode.el' for easy switch-back.
(use-package eglot
  :hook (python-base-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions))
  :config
  ;; Force pyright as the python LSP server. Without this, eglot picks the
  ;; first server on PATH from `eglot-server-programs' — sometimes pylsp,
  ;; jedi-language-server, or ruff-lsp depending on what's installed.
  ;; pyright gives the best type-aware navigation/completion.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode python-base-mode)
                 . ("pyright-langserver" "--stdio")))

  ;; Match the previous lsp-mode bindings — eglot installs an xref backend, so
  ;; these xref commands route through the LSP server when eglot is active.
  (define-key global-map (kbd "M-[") 'xref-find-definitions)
  (define-key global-map (kbd "M-]") 'xref-find-references)
  (define-key global-map (kbd "M-/") 'xref-find-references))

;; flymake (built-in) — eglot's diagnostic surface. Match the previous
;; flycheck bindings for muscle-memory continuity.
(define-key global-map (kbd "C->") 'flymake-goto-next-error)
(define-key global-map (kbd "C-<") 'flymake-goto-prev-error)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
