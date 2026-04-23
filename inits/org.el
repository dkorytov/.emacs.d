(require 'org)

(use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-startup-indented t) 

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "|" "DONE" "|" "DFRD" "CANCELED")))

(setq org-todo-keyword-faces
      `(("DONE" . org-done) ("DOING" . "orange") ("DFRD" . "#696FCD")))

(setq org-agenda-files (list "~/org/projects"))
; log done time
(setq org-log-done 'time)

; Can't finish a task until all sub-tasks are done
(setq org-enforce-todo-dependencies t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-priority-faces
      '((?A . (:foreground "red" :weight bold))
        (?B . (:foreground "dark orange" :weight bold))
        (?C . (:foreground "yellow"))))

(setq org-tag-alist '(("URGENT" . ?u)
                      ("IMPORTANT" . ?i)))


;; Install and enable org-super-agenda
(use-package org-super-agenda
  :ensure t
  :config
  ;; This turns it on globally for all your agenda views
  (org-super-agenda-mode t))

(defun my/org-agenda-relative-date ()
  "Calculate relative days for deadline or scheduled items in non-agenda views."
  (let* ((dl (org-entry-get nil "DEADLINE"))
         (sc (org-entry-get nil "SCHEDULED"))
         (date-str (or dl sc)) ; Deadline takes priority if both exist
         (prefix (if dl "Due" "Sched")))
    (if date-str
        ;; Calculate the difference in days between the task date and today
        (let* ((days (- (org-time-string-to-absolute date-str)
                        (org-time-string-to-absolute (format-time-string "%Y-%m-%d")))))
          (cond
           ((= days 0) (format "%-16s" (format "[%s: Today]" prefix)))
           ((< days 0) (format "%-16s" (format "[%s: %dd late]" prefix (abs days))))
           (t          (format "%-16s" (format "[%s: In %dd]" prefix days)))))
      ;; If no date exists, print 16 blank spaces to keep everything perfectly aligned
      "                ")))

(setq org-agenda-prefix-format
      '((agenda . " %i %-22:c%?-12t% s %(make-string (* 2 (org-outline-level)) 32)")
        (todo   . " %i %-22:c %(my/org-agenda-relative-date) %(make-string (* 2 (org-outline-level)) 32)")
        (tags   . " %i %-22:c %(my/org-agenda-relative-date) %(make-string (* 2 (org-outline-level)) 32)")
        (search . " %i %-22:c")))

(setq org-super-agenda-groups
      '(
	(:name "!! Overdue" ; Name
                :scheduled past ; Filter criteria
                :order 2 ; Order it should appear in agenda view
                :face 'error)

	(:name "⏳ Upcoming Deadlines"
               :deadline future)
               
        (:name "🚀 Scheduled Soon"
               :scheduled future)
	
	;; Group 1: The absolute most critical things
        (:name "🔥 URGENT & CRITICAL"
               :and (:priority "A" :tag "URGENT"))
               
        ;; Group 2: All other P1s that aren't tagged Urgent
        (:name "🚨 High Priority (P1)"
               :priority "A")
               
        ;; Group 3: Items scheduled for specific times today (meetings, etc.)
        (:name "📅 Today's Schedule"
               :time-grid t)
               
        ;; Group 4: Important tasks that aren't P1
        (:name "⭐ Important"
               :tag "IMPORTANT")
               
        ;; Group 5: The default P2 tasks
        (:name "📝 Standard Tasks (P2)"
               :priority "B")
               
        ;; Group 6: The P3 Backlog
        (:name "🧊 Backlog (P3)"
               :priority "C")
               
        ;; The Catch-All: Anything that doesn't fit above gets grouped by its file/project name
        (:auto-category t)))


