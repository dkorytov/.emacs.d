(require 'org)

(use-package org-bullets
    :ensure t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-startup-indented t) 

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "|" "DONE" "|" "DFRD" "CANCELED")))

(setq org-todo-keyword-faces
      `(("DONE" . org-done) ("DOING" . "orange") ("WAITING" . "purple") ("DFRD" . "#696FCD")))

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

(defun my/org-add-created-date ()
  "Automatically add a CREATED property to the current heading."
  (interactive)
  ;; Only run if we are in an Org buffer, the heading has a TODO state, 
  ;; and it doesn't already have a CREATED property.
  (when (and (derived-mode-p 'org-mode)
             (org-get-todo-state)
             (not (org-entry-get nil "CREATED")))
    (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))

;; Trigger when using Shift+Alt+Enter (M-S-RET) to create a new TODO heading
(add-hook 'org-insert-todo-heading-hook 'my/org-add-created-date)

;; Trigger when using Shift+Right to cycle a plain heading into a TODO
(add-hook 'org-after-todo-state-change-hook 'my/org-add-created-date)


;; Define a capture template that automatically injects the creation time
(setq org-capture-templates
      '(("t" "New TODO (Inbox)" entry 
         (file "~/org/projects/inbox.org") ; Make sure to create this file!
         "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i")))

;; Route all archives into the ~/org/projects/archive/ directory.
;; The "%s" dynamically inserts the name of the original file.
;; Example: Archiving from "work.org" sends it to "archive/work.org_archive"
(setq org-archive-location "~/org/projects/archive/%s_archive::")

;; Dynamically use all agenda files as refile targets
(setq org-refile-targets
      '((org-agenda-files :maxlevel . 2) ; Scans every .org file in ~/org/projects
        (nil :maxlevel . 3)))            ; Allows refiling within the current open fileo

;; 2. Make the prompt show the full path (e.g., "cycling.org/Bikepacking Gear")
(setq org-refile-use-outline-path 'file)

;; 3. Allow creating new parent headings on the fly during refile
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Install and enable org-super-agenda
(use-package org-super-agenda
  :ensure t
  :config
  ;; This turns it on globally for all your agenda views
  (org-super-agenda-mode t))

(defun my/org-agenda-tag-markers ()
  "Return 🔥/⭐ markers based on URGENT/IMPORTANT tags on the current agenda entry.
🔥⭐ if both, 🔥 if only URGENT, ⭐ if only IMPORTANT, blanks otherwise (for alignment)."
  (let* ((tags (org-get-tags))
         (urgent    (and tags (member "URGENT" tags)))
         (important (and tags (member "IMPORTANT" tags))))
    (cond
     ((and urgent important) "🔥⭐")
     (urgent                 "🔥  ")
     (important              "⭐  ")
     (t                      "    "))))

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
      '((agenda . " %i %-30:c%?-12t% s %(my/org-agenda-tag-markers) %(make-string (* 2 (org-outline-level)) 32)")
        (todo   . " %i %-30:c %(my/org-agenda-relative-date) %(my/org-agenda-tag-markers) %(make-string (* 2 (org-outline-level)) 32)")
        (tags   . " %i %-30:c %(my/org-agenda-relative-date) %(my/org-agenda-tag-markers) %(make-string (* 2 (org-outline-level)) 32)")
        (search . " %i %-30:c")))

(setq org-super-agenda-groups
      '(
	(:name "⏳ Upcoming Deadlines"
               :deadline future)

        (:name "🚀 Scheduled Soon"
               :scheduled future)
	;; Group 3: Items scheduled for specific times today (meetings, etc.)

	(:name "‼️⏰‼️ Overdue" ; Name
                :scheduled past ; Filter criteria
		:deadline past)

	(:name "🎯 Due Today"
               :deadline today
               :scheduled today)

        ;; Group 0: Both tags — the Eisenhower "do it now" quadrant
        (:name "🔥⭐ Important & Urgent"
               :and (:tag "URGENT" :tag "IMPORTANT"))

	;; Group 1: The absolute most critical things
        (:name "🔥 URGENT"
               :and (:tag "URGENT"))

        ;; Group 7: Important but not P1
        (:name "⭐ Important"
               :tag "IMPORTANT")

        ;; Group 2: All other P1s that aren't tagged Urgent
        (:name "🚨 High Priority (P1)"
               :priority "A")
               
               
        ;; Group 5: The default P2 tasks
        (:name "📝 Standard Tasks (P2)"
               :priority "B")
               
        ;; Group 6: The P3 Backlog
        (:name "🧊 Backlog (P3)"
               :priority "C")
               
        ;; The Catch-All: Anything that doesn't fit above gets grouped by its file/project name
        (:auto-category t)))

(defun my/org-agenda-cmp-closed (a b)
  "Compare two agenda entries A and B by their CLOSED timestamp."
  (let* ((closed-seconds
          (lambda (entry)
            (let ((m (get-text-property 0 'org-marker entry)))
              (and m (org-with-point-at m
                       (when-let ((c (org-entry-get nil "CLOSED")))
                         (org-time-string-to-seconds c)))))))
         (ta (funcall closed-seconds a))
         (tb (funcall closed-seconds b)))
    (cond ((and ta tb) (cond ((> ta tb) 1) ((< ta tb) -1) (t nil)))
          (ta 1)
          (tb -1)
          (t nil))))

(defun my/org-agenda-closed-date ()
  "Return a relative CLOSED date marker for the current agenda entry."
  (let ((c (org-entry-get nil "CLOSED")))
    (if c
        (let ((days (- (org-time-string-to-absolute (format-time-string "%Y-%m-%d"))
                       (org-time-string-to-absolute c))))
          (cond
           ((= days 0) (format "%-16s" "[Today]"))
           ((= days 1) (format "%-16s" "[Yesterday]"))
           (t          (format "%-16s" (format "[%dd ago]" days)))))
      "                ")))

(setq org-agenda-custom-commands
      '(("d" "Recently Closed (last 2 weeks)"
         ((tags "CLOSED>=\"<-2w>\"/DONE|CANCELED|DFRD"
                ((org-agenda-overriding-header "✅ Closed in the last 2 weeks")
                 (org-agenda-cmp-user-defined #'my/org-agenda-cmp-closed)
                 (org-agenda-sorting-strategy '(user-defined-down))
                 (org-agenda-prefix-format
                  '((tags . " %i %-30:c %(my/org-agenda-closed-date) %(my/org-agenda-tag-markers) %(make-string (* 2 (org-outline-level)) 32)")))
                 (org-super-agenda-groups
                  '((:name "✅ Done"     :todo "DONE")
                    (:name "🛑 Canceled" :todo "CANCELED")
                    (:name "💤 Deferred" :todo "DFRD")))))))))

(defun my/sync-secure-mac-calendar ()
  "Pull local macOS calendar into Org natively without the internet."
  (interactive)
  (let ((calendar-file "~/org/projects/calendar.org")
        
        ;; The icalBuddy command. 
        ;; NOTE: If you only want to pull a specific calendar, add: -ic "Work"
        ;; NOTE: If you are on an older Intel Mac, change the path to /usr/local/bin/icalBuddy
        (ical-cmd "/opt/homebrew/bin/icalBuddy -b '* ' -nc -iep 'title,datetime' -po 'title,datetime' -df '%Y-%m-%d %a' -tf '%H:%M' -ps '|::::|' eventsToday+14"))
    
    (with-temp-file calendar-file
      (insert "#+TITLE: Secure Local Mac Calendar\n\n")
      (call-process-shell-command ical-cmd nil t)
      
      ;; 1. Convert the custom '::::' separator into an Org active timestamp
      (goto-char (point-min))
      (while (search-forward "::::" nil t)
        (replace-match "\n  <" t t)
        (end-of-line)
        (insert ">"))
      
      ;; 2. Fix the time ranges (Change "14:00 - 15:00" to "14:00-15:00")
      (goto-char (point-min))
      (while (re-search-forward "\\([0-9]\\{2\\}:[0-9]\\{2\\}\\) - \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)" nil t)
        (replace-match "\\1-\\2" t nil)))
    
    (message "Secure local calendar synced!")))

;; Run automatically every 2 hours in the background
(run-at-time "0 sec" 7200 'my/sync-secure-mac-calendar)
