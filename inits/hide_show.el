(setq hs-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c @ h")   'hs-hide-block)
        (define-key map (kbd "C-c @ s")   'hs-show-block)
        (define-key map (kbd "C-c @ H")  'hs-hide-all)
        (define-key map (kbd "C-c @ S")  'hs-show-all)
        (define-key map (kbd "C-c @ l")   'hs-hide-level)
        (define-key map (kbd "C-c @ @")   'hs-toggle-hiding)
        (define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
	map))

(defadvice goto-line (after expand-after-goto-line
			        activate compile)
  "hideshow-expand affected block when using goto-line in a
collapsed buffer"
  (save-excursion
    (hs-show-block)))

