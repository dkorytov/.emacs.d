(require 'org)

(use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-startup-indented t) 

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "|" "DONE" "|" "DFRD")))

(setq org-todo-keyword-faces
      `(("DONE" . org-done) ("DOING" . "orange") ("DFRD" . "#696FCD")))

(setq org-agenda-files (list "~/org/work.org"
                             "~/org/school.org"
                             "~/org/home.org"))

(setq org-log-done 'time)


