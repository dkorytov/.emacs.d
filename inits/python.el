(use-package numpydoc :ensure t)

(setq python-indent-def-block-scale 1)

;; pet — Python Executable Tracker. Auto-detects per-project venvs from uv,
;; poetry, pipenv, conda, pyenv. Walks up from the buffer's file looking for
;; pyproject.toml and .venv/, then configures `python-shell-interpreter',
;; `flycheck'/`flymake' exec paths, and eglot's pyright `pythonPath' so each
;; tool resolves the right interpreter without any per-project config.
(use-package pet
  :ensure t
  :hook (python-base-mode . pet-mode)
  :config
  ;; Skip pre-commit detection. Pet walks the project tree looking for a
  ;; `.pre-commit-config.yaml' on every python buffer open so it can pin tool
  ;; binaries to the cached pre-commit venvs. We don't use pre-commit, and
  ;; the recursive elisp walk dominates file-open time (profiled at ~150ms).
  ;; Override the predicate to return nil so the search is skipped entirely.
  (advice-add 'pet-use-pre-commit-p :override #'ignore)

  ;; Skip dape (debug adapter) setup. Pet auto-configures dape bindings on
  ;; every python buffer even when dape isn't used (~41ms profiled). Remove
  ;; this override if you start using dape for python debugging.
  (advice-add 'pet-dape-setup :override #'ignore))

;; ruff diagnostics via flymake. eglot's flymake surface already shows pyright
;; (type) diagnostics — flymake-ruff adds ruff's lint diagnostics alongside,
;; so both sources show up under the same C->/C-< navigation bindings.
(use-package flymake-ruff
  :ensure t
  :hook (python-base-mode . flymake-ruff-load))

;; Async format-on-save via apheleia. Use black for formatting (apheleia's
;; default for python-mode is already black, but we set it explicitly for
;; python-base-mode so both python-mode and python-ts-mode buffers use it).
(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'python-base-mode apheleia-mode-alist) '(black))
  (apheleia-global-mode +1))
