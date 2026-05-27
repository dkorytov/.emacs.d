(require 'org)

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-startup-indented t)
(setq org-startup-folded 'content)

(add-hook 'org-mode-hook #'org-fold-hide-drawer-all)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "|" "DONE" "|" "DFRD" "CANCELED")))

(setq org-todo-keyword-faces
      `(("DONE" . org-done) ("DOING" . "orange") ("WAITING" . "purple") ("DFRD" . "#696FCD")))

					; log done time (org-agenda-files set by my/org-apply-context below)
(setq org-log-done 'time)

					; Can't finish a task until all sub-tasks are done
(setq org-enforce-todo-dependencies t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-agenda-window-setup 'current-window
      org-agenda-restore-windows-after-quit t)

(setq org-priority-faces
      '((?A . (:foreground "red" :weight bold))
        (?B . (:foreground "dark orange" :weight bold))
        (?C . (:foreground "yellow"))))

(setq org-tag-alist '(("URGENT" . ?u)
                      ("IMPORTANT" . ?i)
		      ("LIGHT" . ?l)
		      ("FOLLOWUP" . ?f)))

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

(defun my/org-sync-followup-state-at-point ()
  "Sync FOLLOWUP state for the entry at point based on its SCHEDULED date.
- WAITING + scheduled today/past → TODO (now actionable)
- TODO/DOING + scheduled future → WAITING (deferred again)"
  (when (and (derived-mode-p 'org-mode)
             (member "FOLLOWUP" (org-get-tags nil t)))
    (let* ((today (org-time-string-to-absolute (format-time-string "%Y-%m-%d")))
           (state (org-get-todo-state))
           (sched (org-entry-get nil "SCHEDULED"))
           (sched-day (and sched (org-time-string-to-absolute sched))))
      (cond
       ((and sched-day (<= sched-day today)
             (equal state "WAITING"))
        (org-todo "TODO"))
       ((and sched-day (> sched-day today)
             (member state '("TODO" "DOING")))
        (org-todo "WAITING"))))))

(defun my/org-followup-to-waiting ()
  "When FOLLOWUP is added: require a SCHEDULED date, add the LIGHT tag, and
sync state based on the schedule. If the user cancels the schedule prompt,
the FOLLOWUP tag is removed."
  (when (and (derived-mode-p 'org-mode)
             (member "FOLLOWUP" (org-get-tags nil t)))
    (unless (org-entry-get nil "SCHEDULED")
      (condition-case nil
          (org-schedule nil)
        (quit
         (org-toggle-tag "FOLLOWUP" 'off)
         (user-error "FOLLOWUP requires a SCHEDULED date — tag removed"))))
    (unless (member "LIGHT" (org-get-tags nil t))
      (org-toggle-tag "LIGHT" 'on))
    (my/org-sync-followup-state-at-point)))

(add-hook 'org-after-tags-change-hook #'my/org-followup-to-waiting)

(defun my/org-sync-followup-state ()
  "Run `my/org-sync-followup-state-at-point' over every file in `org-agenda-files'."
  (interactive)
  (org-map-entries #'my/org-sync-followup-state-at-point nil 'agenda))

(advice-add 'org-agenda :before
            (lambda (&rest _) (my/org-sync-followup-state)))

(advice-add 'org-schedule :after
            (lambda (&rest _) (my/org-sync-followup-state-at-point)))


;; Capture templates and archive location are set by `my/org-apply-context'
;; below so they switch with the active context (work / home).

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

(defvar my/org-agenda-tag-marker-alist
  '(("URGENT"    . "🔥")
    ("IMPORTANT" . "⭐")
    ("LIGHT"     . "🪶"))
  "Org tag -> emoji marker shown in agenda views.
Order here determines column order; missing tags render as two spaces.")

(defun my/org-agenda-tag-markers ()
  "Return concatenated emoji markers for the current entry's tags.
Each entry in `my/org-agenda-tag-marker-alist' produces its emoji if the
tag is present, otherwise two spaces — keeping agenda columns aligned."
  (let ((tags (org-get-tags)))
    (mapconcat (lambda (pair) (if (member (car pair) tags) (cdr pair) "  "))
               my/org-agenda-tag-marker-alist
               "")))

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

(defun my/org-agenda-cmp-due (a b)
  "Compare two agenda entries by DEADLINE, falling back to SCHEDULED.
Earlier dates sort first; items with neither sort last."
  (let* ((due-seconds
          (lambda (entry)
            (let ((m (get-text-property 0 'org-marker entry)))
              (and m (org-with-point-at m
                       (when-let ((d (or (org-entry-get nil "DEADLINE")
                                         (org-entry-get nil "SCHEDULED"))))
                         (org-time-string-to-seconds d)))))))
         (ta (funcall due-seconds a))
         (tb (funcall due-seconds b)))
    (cond ((and ta tb) (cond ((< ta tb) -1) ((> ta tb) 1) (t nil)))
          (ta -1)
          (tb 1)
          (t nil))))

(setq org-agenda-cmp-user-defined #'my/org-agenda-cmp-due)

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        (todo user-defined-up priority-down category-keep)
        (tags user-defined-up priority-down category-keep)
        (search category-keep)))

(setq org-super-agenda-groups
      '(
	(:name "🪃 Upcoming Followups"
               :and (:tag "FOLLOWUP" :scheduled future))

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

;; ---- Context switching (work / home) ----

(defvar my/org-context-settings
  '((work . ((agenda-files       . ("~/org/projects"))
             (inbox-file         . "~/org/projects/inbox.org")
             (archive-location   . "~/org/projects/archive/%s_archive::")
             (background         . nil)
             (label              . "WORK")))
    (home . ((agenda-files       . ("~/org_home"))
             (inbox-file         . "~/org_home/inbox.org")
             (archive-location   . "~/org_home/archive/%s_archive::")
             (background         . "#2a2520")
             (label              . "HOME"))))
  "Per-context org settings applied by `my/org-apply-context'.")

(defvar my/org-context (if (eq system-type 'darwin) 'work 'home)
  "Currently active org context. Defaults to work on macOS, home elsewhere.")

(defvar my/org-default-background
  (frame-parameter nil 'background-color)
  "Frame background captured before any context tint is applied.")

(defun my/org-context-get (key)
  "Look up KEY in the current `my/org-context'."
  (alist-get key (alist-get my/org-context my/org-context-settings)))

(defun my/org-refresh-agendas ()
  "Refresh every open Org agenda buffer in place."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-redo t)))))

(defun my/org-apply-context ()
  "Apply settings for the current `my/org-context'."
  (setq org-agenda-files     (my/org-context-get 'agenda-files))
  (setq org-archive-location (my/org-context-get 'archive-location))
  (setq org-capture-templates
        `(("t" "New TODO (Inbox)" entry
           (file ,(my/org-context-get 'inbox-file))
           "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %i")))
  (set-background-color (or (my/org-context-get 'background)
                            my/org-default-background))
  (force-mode-line-update t)
  (my/org-sync-followup-state)
  (my/org-refresh-agendas))

(defun my/org-switch-context (name)
  "Switch active org context to NAME (e.g. work or home)."
  (interactive
   (list (intern (completing-read "Context: "
                                  (mapcar (lambda (c) (symbol-name (car c)))
                                          my/org-context-settings)
                                  nil t))))
  (setq my/org-context name)
  (my/org-apply-context)
  (message "Org context: %s" name))

(global-set-key (kbd "C-c o c") #'my/org-switch-context)

(defun my/org-find-project-file ()
  "Open one of the current context's org files via `completing-read'."
  (interactive)
  (let* ((files (org-agenda-files))
         (alist (mapcar (lambda (f) (cons (file-name-base f) f)) files))
         (choice (completing-read "Org file: " alist nil t)))
    (find-file (cdr (assoc choice alist)))))

(global-set-key (kbd "C-c o f") #'my/org-find-project-file)

(defvar my/org-context-mode-line
  '(:eval (format " [%s]" (my/org-context-get 'label)))
  "Mode line construct showing the active org context.")

(unless (member my/org-context-mode-line global-mode-string)
  (setq global-mode-string
        (append (or global-mode-string '("")) (list my/org-context-mode-line))))

(my/org-apply-context)
