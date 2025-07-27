;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic Emacs Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(tool-bar-mode -1)
(put 'upcase-region 'disabled nil)
(setq inhibit-startup-message t)
(menu-bar-mode -99)
(set-default 'truncate-lines t)
(setq-default fill-column 119)
(setq column-number-mode t)
(put 'downcase-region 'disabled nil)
(setq confirm-kill-emacs 'y-or-n-p)

;;a clear function for eshell
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;;only kills/copies text if the area is active
(setq mark-even-if-inactive nil)


(defun prev-window ()
  "Return to the previous window."
   (interactive)
   (other-window -1))

(define-key global-map (kbd "C-x p") 'prev-window)
(define-key global-map (kbd "<f11>") 'toggle-full-screen)
(define-key global-map (kbd "M-<down>") 'scroll-up-line)
(define-key global-map (kbd "M-<up>") 'scroll-down-line)
(define-key global-map (kbd "M-n") 'scroll-up-line)
(define-key global-map (kbd "M-p") 'scroll-down-line)
(define-key global-map (kbd "M-:") 'comment-box)


(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
	;; This would override `fill-column' if it's an integer.
	(emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'unfill-paragraph)

(defun td ()
    "Writes out a todo line with the current date."
    (interactive)
    (insert "# TODO Dan Korytov [")
    (insert (format-time-string "%Y-%m-%d"))
    (insert "]: "))

(defun nt ()
    "Writes out a todo line with the current date."
    (interactive)
    (insert "# Note Dan Korytov [")
    (insert (format-time-string "%Y-%m-%d"))
    (insert "]: "))

(defun rb ()
  "Rerverts buffer."
  (interactive)
  (revert-buffer))

(defun rby ()
  "Rerverts buffer with auto yes."
  (interactive)
  (revert-buffer nil `true))

(defalias 'wc 'count-words)

;; Add git branch to eshell prompt
;; ref: https://superuser.com/questions/890937/how-to-show-git-branch-in-emacs-shell
(defun git-prompt-branch-name ()
    "Get current git branch name."
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))

(defun dkorytov:eshell-prompt ()
  "Prompt for eshell with git branch."
  (let ((branch-name (git-prompt-branch-name)))
    (concat
     (if branch-name (format "\n[%s]\n" branch-name) "\n")
     (abbreviate-file-name (eshell/pwd)) " $ "
     )))

(setq eshell-prompt-function #'dkorytov:eshell-prompt
      eshell-prompt-regexp ".*$+ ")

(setq bell-volume 0)
(setq visible-bell 1)
