;; From https://github.com/cofi/dotfiles/blob/master/emacs.d/

(setq min-window-width (* 2 81))

(defun cofi/smart-split ()
  "Split window vertically or horizontally in a smart way."
  (interactive)
  (if (or (< (frame-width) min-window-width)
          (< (window-width) min-window-width))
      (split-window-vertically)
    (split-window-horizontally)))

;; Add narrow-or-widen-dwim
;; From: http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html.
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
  Intelligently means: region, org-src-block, org-subtree, or defun,
  whichever applies first.
  Narrowing to org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer is already
  narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((org-in-src-block-p)
                (org-edit-src-code)
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

;;; Org-mode
(defun op/load-org-agenda-full-screen (prefix agenda-type)
  "Opens up the agenda view of `agenda-type'. If called with
  prefix-argument opens up the agenda view in full-screen."
  (if (equal prefix 1)
      (org-agenda '(4) agenda-type)
    (progn
      (org-agenda '(4) agenda-type)
      (delete-other-windows))))

(defun op/load-agenda-full-screen (arg)
  (interactive "p")
  (if (equal arg 4)
      (org-agenda '(4) " ")
    (op/load-org-agenda-full-screen 4 " ")))

(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/.emacs.d/private/org-style.css" path)))
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))


;; Extracts the link location from an org-link to the kill-ring
(defun op/org-extract-link ()
  "Extract the link location at point and put it on the killring."
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (kill-new (org-link-unescape (org-match-string-no-properties
                                  1)))))


;; Getting Things Done in Org-Mode
;; Largely stolen from: http://doc.norang.ca/org-mode.html

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; Project definition and finding stuck projects
(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
  Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
    This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
    This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)

                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT\\|WAITING " subtree-end t))
                (setq has-next t)))
            (if (or has-next (member "DEFER" (org-get-tags-at)))
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((and (bh/is-project-p)
                 (marker-buffer org-agenda-restrict-begin))
            nil)
           ((and (bh/is-project-p)
                 (not (marker-buffer org-agenda-restrict-begin))
                 (not (bh/is-project-subtree-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
  When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
  When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (daynr (string-to-int (format-time-string "%d" (current-time))))
                 (a-month-ago (* 60 60 24 (+ daynr 1)))
                 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                 (this-month (format-time-string "%Y-%m-" (current-time)))
                 (subtree-is-current (save-excursion
                                       (forward-line 1)
                                       (and (< (point) subtree-end)
                                            (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
            (if subtree-is-current
                next-headline ; Has a date in this month or last month, skip it
              nil))           ; available to archive
        (or next-headline (point-max))))))

;; Custom org-agenda commands/functions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
  I don't use follow mode very often but I restrict to file all the time
  so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (org-get-at-bol 'org-hd-marker)
      (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(defvar bh/project-list nil)

;; (defvar bh/current-view-project nil)

;; (defun bh/view-next-project ()
;;   (interactive)
;;   (unless (marker-position org-agenda-restrict-begin)
;;     (goto-char (point-min))
;;     (setq bh/current-view-project (point)))
;;   (bh/widen)
;;   (goto-char bh/current-view-project)
;;   (forward-visible-line 1)
;;   (while (and (< (point) (point-max))
;;               (or (not (org-get-at-bol 'org-hd-marker))
;;                   (org-with-point-at (org-get-at-bol 'org-hd-marker)
;;                     (or (not (bh/is-project-p))
;;                         (bh/is-project-subtree-p)))))
;;     (forward-visible-line 1))
;;   (setq bh/current-view-project (point))
;;   (if (org-get-at-bol 'org-hd-marker)
;;       (progn
;;         (bh/narrow-to-project)
;;         (org-agenda-redo)
;;         (beginning-of-buffer))
;;     (beginning-of-buffer)
;;     (error "All projects viewed.")))

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
                                        ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

                                        ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

                                        ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
                                        ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;; Clocking
(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))


(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find op/doing-work-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)


(defun org-check-misformatted-subtree ()
  "Check misformatted entries in the current buffer."
  (interactive)
  (show-all)
  (org-map-entries
   (lambda ()
     (when (and (move-beginning-of-line 2)
                (not (looking-at org-heading-regexp)))
       (if (or (and (org-get-scheduled-time (point))
                    (not (looking-at (concat "^.*" org-scheduled-regexp))))
               (and (org-get-deadline-time (point))
                    (not (looking-at (concat "^.*" org-deadline-regexp)))))
           (when (y-or-n-p "Fix this subtree? ")
             (message "Call the function again when you're done fixing this subtree.")
             (recursive-edit))
         (message "All subtrees checked."))))))

;; Aligns all tables in a file
(defun my-align-all-tables ()
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))


;; Removes redundant tags
(defun org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.

    A tag is considered redundant if it is local to a headline and
    inherited by a parent headline."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       '(lambda ()
          (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
                local inherited tag)
            (dolist (tag alltags)
              (if (get-text-property 0 'inherited tag)
                  (push tag inherited) (push tag local)))
            (dolist (tag local)
              (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))


:;; Emacs Lisp

(defun op/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;;; Misc

(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun op/copy-last-message (p)
  "Copies the last line of *Messages* to kill-ring. Will strip
  the [times N] string at the end of the line for commands that
  were repeated.

  With prefix P, insert the last message to the current buffer."
  (interactive "P")
  (let ((cur-buf (current-buffer)))
    (save-excursion
      (set-buffer "*Messages*")
      (goto-char (point-max))
      (beginning-of-line)
      (when (looking-at "^$")
        (forward-line -1))
      (let ((txt (replace-regexp-in-string
                  " \\[[0-9]+ times\\]$" ""
                  (buffer-substring (line-beginning-position)
                                    (line-end-position)))))
        (if p
            (with-current-buffer cur-buf (insert txt))
          (kill-new txt))))))


(defun swap-window (direction)
  "Swap current window with the one in `direction'."
  (interactive (list (ido-completing-read "Swap with window: "
                                          (mapcar 'symbol-name
                                                  '(left right down up)))))
  (let* ((dir (if (symbolp direction)
                  direction
                (intern direction)))
         (other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window (selected-window))
             (this-buffer (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start (window-start this-window))
             (other-start (window-start other-window)))
        (set-window-buffer this-window other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start this-window other-start)
        (set-window-start other-window this-start)))))

(defun swap-with-left () (interactive) (swap-window 'left))
(defun swap-with-down () (interactive) (swap-window 'down))
(defun swap-with-up () (interactive) (swap-window 'up))
(defun swap-with-right () (interactive) (swap-window 'right))

(defun luxbock/set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(defun luxbock/switch-to-default-agenda-view (arg)
  (interactive "p")
  (let ((b (get-buffer "*Org Agenda( )*")))
    (if b (switch-to-buffer b)
      (execute-kbd-macro (kbd "C-c a SPC"))
      (switch-to-buffer b))))
