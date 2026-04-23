(use-package numpydoc :ensure t)

(setq python-indent-def-block-scale 1)

(defun setup-flycheck-python-project-path ()
  (interactive)
  (let ((root (ignore-errors (projectile-project-root))))
    (when root
      (add-to-list
       (make-variable-buffer-local 'flycheck-python-import-path)
       root))))

(add-hook 'python-mode-hook 'setup-flycheck-python-project-path)
(add-hook 'python-mode-hook #'(lambda () (setq flycheck-checker 'python-pylint)))
					;(flycheck-add-next-checker 'lsp 'python-pylint 'python-pyright)
;; (setq flycheck-python-pylint-executable "pylint")
;; (add-hook 'python-mode '(lambda ()
;; 			  (semantic-mode 1)
;; 			  (setq flycheck-checker 'python-pylint)))
