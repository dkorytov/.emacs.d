;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-sitter configuration (built-in `treesit', Emacs 29+)
;;
;; This file does three things:
;;   1. Tells `treesit' where to download grammars (`treesit-language-source-alist').
;;   2. Auto-routes classic major modes to their `*-ts-mode' variants.
;;   3. Installs missing grammars in the background and wires up `treesit-fold'
;;      for syntax-aware code folding (replacing the regex-based `hs-minor-mode').
;;
;; Compiled grammars live under ~/.emacs.d/tree-sitter/.
;; To install one manually: M-x treesit-install-language-grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;----------------------------------------------------------------------------
;; Grammar sources
;;----------------------------------------------------------------------------
;; Each entry is (LANG REPO-URL [BRANCH] [SUBDIR]). `treesit-install-language-grammar'
;; reads this list to know where to clone+compile grammars from. JS/TS need a
;; subdir because the upstream repo nests grammar source under a folder.
(setq treesit-language-source-alist
      '((python     "https://github.com/tree-sitter/tree-sitter-python")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml")
        (toml       "https://github.com/tree-sitter-grammars/tree-sitter-toml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (cmake      "https://github.com/uyha/tree-sitter-cmake")
        (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
        (c          "https://github.com/tree-sitter/tree-sitter-c")
        (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")))


;;----------------------------------------------------------------------------
;; Auto-route classic modes to their tree-sitter variants
;;----------------------------------------------------------------------------
;; When Emacs picks a major mode for a buffer (via auto-mode-alist, etc.) it
;; consults `major-mode-remap-alist' first. Listing `python-mode . python-ts-mode'
;; means opening a .py file lands in `python-ts-mode' instead of the legacy mode,
;; which gives us tree-sitter-aware indentation, font-lock, navigation, and folding.
;; Note: there is no `emacs-lisp-ts-mode' in core, so .el files stay on the
;; classic `emacs-lisp-mode' (and use `hs-minor-mode' for folding).
(setq major-mode-remap-alist
      '((python-mode     . python-ts-mode)
        (sh-mode         . bash-ts-mode)
        (bash-mode       . bash-ts-mode)
        (js-mode         . js-ts-mode)
        (js-json-mode    . json-ts-mode)
        (yaml-mode       . yaml-ts-mode)
        (conf-toml-mode  . toml-ts-mode)
        (css-mode        . css-ts-mode)))


;;----------------------------------------------------------------------------
;; Auto-install missing grammars (deferred, concurrent)
;;----------------------------------------------------------------------------
;; First time Emacs starts, no grammars are compiled yet — opening a .py file
;; would land in `python-ts-mode' but font-lock would silently fail. Rather than
;; require manual installs, we walk `treesit-language-source-alist' on idle and
;; install whatever's missing.
;;
;; Concurrency caveat: each thread calls `treesit-install-language-grammar',
;; which shells out to git+cc via blocking `call-process'. Threads dispatch in
;; parallel but the underlying subprocess calls largely serialize. Good enough
;; for a one-time install — true parallelism would require `make-process' +
;; sentinels, which isn't worth the complexity here.
(defun dkorytov:treesit-ensure-grammars ()
  "Install any missing tree-sitter grammars in background threads.
Errors are swallowed per-grammar so one failure (e.g. repo moved, no
network) doesn't abort the rest."
  (dolist (entry treesit-language-source-alist)
    (let ((lang (car entry)))
      (unless (treesit-language-available-p lang)
        (make-thread
         (lambda ()
           (condition-case err
               (progn
                 (message "treesit: installing %s..." lang)
                 (treesit-install-language-grammar lang)
                 (message "treesit: installed %s" lang))
             (error (message "treesit: failed to install %s: %S" lang err))))
         (format "treesit-install-%s" lang))))))

;; Run 3s after Emacs is idle — keeps startup snappy and ensures the user is
;; already past `after-init-hook' before we kick off compilation work.
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (run-with-idle-timer 3 nil #'dkorytov:treesit-ensure-grammars))


;;----------------------------------------------------------------------------
;; Code folding via `treesit-fold'
;;----------------------------------------------------------------------------
;; `treesit-fold' uses tree-sitter's parsed AST to fold by syntactic node
;; (function, class, block, multi-line string, etc.) rather than regex-matching
;; like the built-in `hs-minor-mode'. It only works in `*-ts-mode' buffers, so
;; classic modes (including emacs-lisp-mode) continue to use `hs-minor-mode'.
;;
;; Other useful commands (unbound — call via M-x or add to :bind):
;;   treesit-fold-open                — open fold at point only
;;   treesit-fold-close               — close fold at point only
;;   treesit-fold-open-recursively    — open fold and all nested folds
;;   treesit-fold-indicators-mode     — show fold markers in the left fringe
(use-package treesit-fold
  :ensure t
  :bind (:map treesit-fold-mode-map
              ;; "C-c <letter>" is reserved for users in the Emacs key conventions,
              ;; so these won't collide with mode-defined bindings.
              ("C-c f"   . treesit-fold-toggle)
              ("C-c F"   . treesit-fold-open-all)
              ("C-c M-f" . treesit-fold-close-all))

  ;; The package's `defface' uses `:box (:style pressed-button)' which renders
  ;; the placeholder as a 3D button. Override the spec to inherit `default'
  ;; for a flat, unstyled look.
  :custom-face
  (treesit-fold-replacement-face ((t (:inherit default))))
  (treesit-fold-replacement-mouse-face ((t (:inherit default))))

  :config
  ;; `:custom-face' alone wasn't enough — gruvbox (or some other theme layer)
  ;; was re-applying the `:box' attribute. `set-face-attribute' runs after the
  ;; package loads and explicitly clears every cosmetic attribute, which sticks.
  (set-face-attribute 'treesit-fold-replacement-face nil
                      :inherit 'default
                      :box nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :weight 'normal
                      :slant 'normal
                      :underline nil)
  (set-face-attribute 'treesit-fold-replacement-mouse-face nil
                      :inherit 'default
                      :box nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :weight 'normal
                      :slant 'normal
                      :underline nil)

  ;; Strip click-to-unfold behavior from the placeholder.
  ;; treesit-fold's `--create-overlay' propertizes the display string with
  ;; `mouse-face', a `keymap' bound to <mouse-1>, and a `help-echo' tooltip,
  ;; making the placeholder behave like a button. We want plain inert text —
  ;; folds open via `C-c f' / `M-x treesit-fold-toggle' instead.
  (defun dkorytov:treesit-fold-strip-mouse (ov &rest _)
    "Remove mouse-interaction text properties from a fold overlay's display string."
    (when (and (overlayp ov) (overlay-get ov 'display))
      (let ((d (overlay-get ov 'display)))
        (when (stringp d)
          (remove-text-properties 0 (length d)
                                  '(mouse-face nil keymap nil help-echo nil)
                                  d))))
    ov)
  ;; Two advice points: `--create-overlay' creates the overlay initially;
  ;; `--hide-ov' rebuilds the display string when a fold is re-hidden after
  ;; being temporarily shown (e.g. by isearch).
  (advice-add 'treesit-fold--create-overlay :filter-return
              #'dkorytov:treesit-fold-strip-mouse)
  (advice-add 'treesit-fold--hide-ov :after
              #'dkorytov:treesit-fold-strip-mouse)

  ;; Custom placeholder character.
  ;; treesit-fold has no `defcustom' for the no-summary fallback — line 421 of
  ;; treesit-fold.el hard-codes `(truncate-string-ellipsis)' (yields "..." or "…").
  ;; We advise the formatter to swap that one case for our arrow. Folds that
  ;; *do* have summary content (e.g. docstring first line) are unaffected.
  (defvar dkorytov:treesit-fold-placeholder " ⤵"
    "Replacement text shown when a fold has no summary content.")
  (defun dkorytov:treesit-fold-replacement-advice (orig-fun beg end)
    (let ((result (funcall orig-fun beg end)))
      (if (string= result (truncate-string-ellipsis))
          dkorytov:treesit-fold-placeholder
        result)))
  (advice-add 'treesit-fold--format-overlay-text :around
              #'dkorytov:treesit-fold-replacement-advice)

  ;; Enable folding in every ts-mode we have a grammar for. Languages without
  ;; a ts-mode (or without a grammar installed) are simply not listed.
  :hook ((python-ts-mode     . treesit-fold-mode)
         (bash-ts-mode       . treesit-fold-mode)
         (js-ts-mode         . treesit-fold-mode)
         (json-ts-mode       . treesit-fold-mode)
         (yaml-ts-mode       . treesit-fold-mode)
         (toml-ts-mode       . treesit-fold-mode)
         (css-ts-mode        . treesit-fold-mode)
         (typescript-ts-mode . treesit-fold-mode)
         (tsx-ts-mode        . treesit-fold-mode)
         (go-ts-mode         . treesit-fold-mode)
         (rust-ts-mode       . treesit-fold-mode)
         (c-ts-mode          . treesit-fold-mode)
         (c++-ts-mode        . treesit-fold-mode)))
