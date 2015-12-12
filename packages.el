(setq luxbock-packages
  '(org
    ox-gfm
    ace-jump-mode
    imenu-anywhere
    clojure-mode-extra-font-locking
    visual-regexp-steroids
    dired-subtree
    dired-filter
    git-auto-commit-mode
    drag-stuff
    tiny
    htmlize
    multiple-cursors
    rainbow
    paxedit
    edn
    typed-clojure-mode
    scratch
    feature-mode
    evil-cleverparens))

(defun luxbock/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :defer t
    :init
    (progn
      (setq org-log-done t)
      (setq org-agenda-window-setup 'current-window)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "k" 'outline-previous-visible-heading
        "j" 'outline-next-visible-heading
        "p" 'org-priority
        "h" 'outline-up-heading
        "y" 'op/org-extract-link
        "c" 'org-capture
        "d" 'org-deadline
        "e" 'org-export-dispatch
        "i" 'org-clock-in
        "o" 'org-clock-out
        "m" 'org-ctrl-c-ctrl-c
        "r" 'org-refile
        "s" 'org-schedule
        "t" 'org-todo)

      (add-hook 'org-mode-hook 'org-indent-mode)
      (add-hook 'org-mode-hook 'auto-fill-mode)
      (add-hook 'org-capture-mode-hook 'evil-insert-state)

      (eval-after-load "org-agenda"
        '(progn
           (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
           (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
           (define-key org-agenda-mode-map
             (kbd "SPC") spacemacs-default-map))))

    :config
    (progn
      (require 'org)
      (require 'org-habit)
      (require 'org-protocol)
      (require 'org-id)

      ;; Custom CSS for org-export
      (eval-after-load 'ox
        '(progn
           (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)))

      (global-set-key (kbd "C-c l") 'org-store-link)
      (global-set-key (kbd "C-c a") 'org-agenda)
      (global-set-key (kbd "C-c c") 'org-capture)

      (define-key org-mode-map (kbd "C-c k") 'org-cut-subtree)
      (define-key org-mode-map (kbd "M-j") 'org-metadown)
      (define-key org-mode-map (kbd "M-k") 'org-metaup)
      (define-key org-mode-map (kbd "M-h") 'org-metaleft)
      (define-key org-mode-map (kbd "M-l") 'org-metaright)
      (define-key org-mode-map (kbd "M-J") 'org-shiftmetadown)
      (define-key org-mode-map (kbd "M-K") 'org-shiftmetaup)
      (define-key org-mode-map (kbd "M-H") 'org-shiftmetaleft)
      (define-key org-mode-map (kbd "M-L") 'org-shiftmetaright)

      ;; Open normal agenda view
      (global-set-key (kbd "<f9> SPC")
                      (lambda (arg)
                        (interactive "p")
                        (op/load-org-agenda-full-screen arg " ")))

      (global-set-key (kbd "<f8>") 'op/load-agenda-full-screen)

      ;; Open just TODO-items
      (global-set-key (kbd "<f9> T")
                      (lambda (arg)
                        (interactive "p")
                        (op/load-org-agenda-full-screen arg "T")))

      ;; Open Habits
      (global-set-key (kbd "<f9> h")
                      (lambda (arg)
                        (interactive "p")
                        (op/load-org-agenda-full-screen arg "h")))

      ;; Askf for a tag to filter for
      (global-set-key (kbd "<f9> m")
                      (lambda (arg)
                        (interactive "p")
                        (op/load-org-agenda-full-screen arg "m")))

      ;; Open notes
      (global-set-key (kbd "<f9> N")
                      (lambda (arg)
                        (interactive "p")
                        (op/load-org-agenda-full-screen arg "N")))


      (use-package org-bullets
        :config
        (defun spacemacs//org-mode-hook ()
          (org-bullets-mode 1))
        (add-hook 'org-mode-hook 'spacemacs//org-mode-hook))

      (setq org-startup-indented t
            org-cycle-separator-lines 0
            org-hide-emphasis-markers t)

      (setq org-modules (quote (org-bbdb
                                org-bibtex
                                org-crypt
                                org-drill
                                org-jsinfo
                                org-w3m
                                org-protocol
                                org-habit)))

      ;; Org-Agenda
      (setq org-directory "~/org"
            org-agenda-files '("~/org/gtd/" "~/org/projects.org")
            someday-file '("~/org/someday.org")
            weblog-file '("~/org/weblog.org")
            org-default-notes-file "~/org/gtd/refile.org"
            org-use-fast-todo-selection t
            org-clock-persist t
            org-return-follows-link t
            org-log-done 'time
            org-log-into-drawer t
            org-log-state-notes-insert-after-drawers nil)

      (setq org-priority-faces
            '(("A" . org-priority)
              ("B" . org-priority)
              ("C" . org-priority)))

;;; Clocking

      ;; Resume clocking task when emacs is restarted
      (org-clock-persistence-insinuate)
      ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
      (setq org-clock-history-length 23)
      ;; Resume clocking task on clock-in if the clock is open
      (setq org-clock-in-resume t)
      ;; Change tasks to NEXT when clocking in
      (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
      ;; Separate drawers for clocking and logs
      (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
      ;; Save clock data and state changes and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t)
      ;; Clock out when moving task to a done state
      (setq org-clock-out-when-done t)
      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      (setq org-clock-persist t)
      ;; Do not prompt to resume an active clock
      (setq org-clock-persist-query-resume nil)
      ;; Enable auto clock resolution for finding open clocks
      (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
      ;; Include current clocking task in clock reports
      (setq org-clock-report-include-clocking-task t)

      (setq op/keep-clock-running nil)

      (defvar op/doing-work-task-id "958228FE-4DBF-444C-8BD2-8AC1AD4A2077")

      ;; Org-Babel
      (eval-after-load 'org-src
        '(progn
           (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)
           (define-key org-src-mode-map (kbd "C-x C-s") #'org-edit-src-exit)))

      (defun bh/display-inline-images ()
        (condition-case nil
            (org-display-inline-images)
          (error nil)))

      (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((emacs-lisp . t)
                                     (dot . t)
                                     (ditaa . t)
                                     (ledger . t)
                                     (python . t)
                                     (clojure . t)
                                     (sh . t)
                                     (org . t)))

      ;; From http://www.wisdomandwonder.com/link/9008/a-progress-indicator-for-code-blocks-in-org-mode
      (defadvice org-babel-execute-src-block (around progress nil activate)
        (set-face-attribute
         'org-block-background nil :background "medium-blue")
        (message "Running your code block")
        ad-do-it
        (set-face-attribute 'org-block-background nil :background "gray12")
        (message "Done with code block"))

      (require 'ob)
      (require 'ob-clojure)

      (setq org-src-fontify-natively t
            org-confirm-babel-evaluate nil
            org-export-babel-evaluate nil
            org-src-window-setup 'current-window)

      (setq org-babel-clojure-backend 'cider)

      ;; Time Stamps
      (defvar bh/insert-inactive-timestamp t)

      (defun bh/toggle-insert-inactive-timestamp ()
        (interactive)
        (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
        (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

      (defun bh/insert-inactive-timestamp ()
        (interactive)
        (org-insert-time-stamp nil t t nil nil nil))

      (defun bh/insert-heading-inactive-timestamp ()
        (save-excursion
          (when bh/insert-inactive-timestamp
            (org-return)
            (org-cycle)
            (bh/insert-inactive-timestamp))))

      (add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

      ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "oi" 'bh/insert-inactive-timestamp)
      (global-set-key (kbd "<f9> t") 'bh/toggle-insert-inactive-timestamp)

      ;; Agenda Mode Keys
      (bind-keys :map org-agenda-mode-map
                 ("V" . bh/view-next-project)
                 ("F" . bh/restrict-to-file-or-follow)
                 ("W" . bh/widen)
                 ("P" . bh/narrow-to-project)
                 ("U" . bh/narrow-up-one-level)
                 ("N" . bh/narrow-to-subtree))


      ;; Tags
      (setq org-tag-alist '((:startgroup)
                            ("@ERRAND" . ?e)
                            ("@ABROAD" . ?a)
                            ("@OFFLINE" . ?o)
                            ("@INTERNET" . ?i)
                            (:endgroup)
                            ("APPOINTMENT" . ?A)
                            ("PROJECT" . ?j)
                            ("BUY" . ?b)
                            ("drill" . ?d)
                            ("private" . ?v)
                            ("PAY" . ?p)
                            ("BANKING" . ?B)
                            ("RESERVE" . ?r)
                            ("PLANNING" . ?l)
                            ("EMAIL" . ?E)
                            ("PHONE" . ?h)
                            ("POKER" . ?P)
                            ("ANKI" . ?n)
                            ("EMACS" . ?m)
                            ("PROGRAMMING" . ?c)
                            ("GEEK" . ?g)
                            ("READING" . ?R)
                            ("STUDY" . ?s)
                            ("FAMILY" . ?y)
                            ("FRIENDS" . ?f)
                            ("GF" . ?G)))

      ;; Keywords for TODO
      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                    (sequence "WAITING(w/!)" "DEFER(f@/!)" "|" "CANCELLED(c/!)" ))))

      ;; (setq org-todo-keyword-faces
      ;;       (quote (("TODO" :foreground "RosyBrown1" :weight bold)
      ;;               ("NEXT" :foreground "SpringGreen1" :weight bold)
      ;;               ("DONE" :foreground "forest green" :weight bold)
      ;;               ("WAITING" :foreground "orange" :weight bold)
      ;;               ("DEFER" :foreground "maroon1" :weight bold)
      ;;               ("CANCELLED" :foreground "forest green" :weight bold))))

      (setq org-todo-state-tags-triggers
            (quote (("CANCELLED" ("CANCELLED" . t))
                    ("WAITING" ("WAITING" . t))
                    ("DEFER" ("WAITING" . t) ("DEFER" . t))
                    (done ("WAITING") ("DEFER"))
                    ("TODO" ("WAITING") ("CANCELLED") ("DEFER"))
                    ("NEXT" ("WAITING") ("CANCELLED") ("DEFER"))
                    ("DONE" ("WAITING") ("CANCELLED") ("DEFER")))))

      ;; Org-Capture
      (setq org-capture-templates
            `(
              ("t" "Todo"
               entry
               (file "~/org/gtd/refile.org")
               "* TODO %?\n%U\n")
              ("n" "Note"
               entry
               (file "~/org/gtd/refile.org")
               "* %? :NOTE:\n%U\n")
              ("i" "Idea"
               entry
               (file "~/org/ideas.org")
               "\n* %? :IDEA:\n%U\n")
              ("j" "Journal"
               entry
               (file+datetree "~/org/journal.org.gpg")
               "* %? :JOURNAL:\n")
              ("s" "Daily Synopsis"
               entry
               (file+datetree "~/org/journal.org.gpg")
               "* Synopsis :drill:\n%?\n** Date:\n%<%e %B %Y, %A>")
              ("d" "Drill"
               entry ;; use plain items so that I can write
               (file "~/org/drill/refile.org") ;; the drill-items via yasnippet-templates
               "* %?"
               :empty-lines 1)
              ("l" "Ledger Entry" plain (file "~/ledger/2015.ledger")
               "%(org-read-date) * %?\n"
               :empty-lines 1)

              ;; ("p" "org-protocol"
              ;;  entry (file "~/org/gtd/refile.org")
              ;;  "* TODO Check out: %c :URL:\n%U\n" :immediate-finish t)
              ("w" "Web"
               entry
               (file "~/org/gtd/refile.org")
               "* TODO Check out: %? :URL:\n%U\n")))

      ;; Refiling
      (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                       (org-agenda-files :maxlevel . 9)
                                       (someday-file :maxlevel . 9)
                                       (weblog-file :maxlevel . 9))))

      (setq org-refile-use-outline-path t
            org-outline-path-complete-in-steps nil
            org-refile-allow-creating-parent-nodes 'confirm
            org-refile-target-verify-function 'bh/verify-refile-target)

      ;; Effort
                                        ; Set default column view headings: Task Effort Clock_Summary
      (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

                                        ; global Effort estimate values
                                        ; global STYLE property values for completion
      (setq org-global-properties '(("Effort_ALL" . "0:05 0:10 0:15 0:30 0:45 1:00 2:00 3:00 5:00 0:00")
                                    ; ("STYLE_ALL" . "habit")
                                    ))

      ;; Agenda View -- LONG
      ;; How the windows get split
      (setq org-agenda-window-setup 'other-window)

      ;; Dim blocked tasks
      (setq org-agenda-dim-blocked-tasks t)

      ;; Compact the block agenda view
      (setq org-agenda-compact-blocks t)

      ;; Default view in the agenda view
      (setq org-agenda-span 'day)

      (setq org-stuck-projects (quote ("" nil nil "")))

      ;; Need this so that scheduled, deadlined or timestamped tasks don't
      ;; show up in the custom agenda block
      (setq org-agenda-tags-todo-honor-ignore-options t)

      ;; custom agenda command definitions
      (setq org-agenda-custom-commands
            (quote (("N" "Notes" tags "NOTE"
                     ((org-agenda-overriding-header "Notes")
                      (org-tags-match-list-sublevels t)))
                    (" " "Agenda"
                     ((agenda "" nil)
                      (tags-todo "-WAITING-CANCELLED/!NEXT"
                                 ((org-agenda-overriding-header "Next Tasks")
                                  (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                  (org-agenda-todo-ignore-scheduled t)
                                  (org-agenda-todo-ignore-deadlines t)
                                  (org-agenda-todo-ignore-with-date t)
                                  (org-agenda-tags-todo-honor-todo-options t)
                                  (org-tags-match-list-sublevels t)
                                  (org-agenda-sorting-strategy
                                   '(todo-state-down priority-down effort-up category-keep))))
                      (tags-todo "-REFILE-CANCELLED-URL/!-DEFER-WAITING"
                                 ((org-agenda-overriding-header "Tasks")
                                  (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                                  (org-agenda-todo-ignore-scheduled t)
                                  (org-agenda-todo-ignore-deadlines t)
                                  (org-agenda-todo-ignore-with-date t)
                                  (org-agenda-sorting-strategy
                                   '(priority-down effort-up category-keep))))
                      (tags-todo "-CANCELLED/!-DEFER+WAITING"
                                 ((org-agenda-overriding-header "Waiting")
                                  (org-tags-match-list-sublevels nil)
                                  (org-agenda-todo-ignore-scheduled 'future)
                                  (org-agenda-todo-ignore-deadlines 'future)))
                      (tags-todo "-CANCELLED/!"
                                 ((org-agenda-overriding-header "Stuck Projects")
                                  (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                      (tags-todo "-DEFER-CANCELLED/!"
                                 ((org-agenda-overriding-header "Projects")
                                  (org-agenda-skip-function 'bh/skip-non-projects)
                                  (org-agenda-sorting-strategy
                                   '(priority-down effort-up category-keep))))
                      (tags-todo "-CANCELLED/!DEFER"
                                 ((org-agenda-overriding-header "Deferred")
                                  (org-agenda-skip-function 'bh/skip-stuck-projects)
                                  (org-tags-match-list-sublevels nil)
                                  (org-agenda-todo-ignore-scheduled 'future)
                                  (org-agenda-todo-ignore-deadlines 'future)))
                      (tags-todo "-REFILE+URL-DONE"
                                 ((org-agenda-overriding-header "Web")
                                  (org-tags-match-list-sublevels nil)))
                      (tags "REFILE"
                            ((org-agenda-overriding-header "Tasks to Refile")
                             (org-tags-match-list-sublevels nil)))
                      (tags "-REFILE/"
                            ((org-agenda-overriding-header "Tasks to Archive")
                             (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                             (org-tags-match-list-sublevels nil))))
                     nil)
                    ("r" "Tasks to Refile" tags "REFILE"
                     ((org-agenda-overriding-header "Tasks to Refile")
                      (org-tags-match-list-sublevels nil)))
                    ("d" "Done Tasks" agenda ""
                     ((org-agenda-start-with-log-mode '(closed))
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'nottodo '("DONE")))))
                    ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                    ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-agenda-todo-ignore-with-date t)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
                    ("t" "Tasks" tags-todo "-REFILE-CANCELLED/!-DEFER-WAITING"
                     ((org-agenda-overriding-header "Tasks")
                      (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
                    ("p" "Projects" tags-todo "-DEFER-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'bh/skip-non-projects)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
                    ("w" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
                     ((org-agenda-overriding-header "Waiting and Postponed tasks"))
                     (org-tags-match-list-sublevels nil))
                    ("h" "Habits" tags-todo "STYLE=\"habit\""
                     ((org-agenda-overriding-header "Habits")
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
                    ("u" "Web" tags-todo "+URL-DONE"
                     ((org-agenda-overriding-header "List of pages to study/read later")))
                    ("A" "Tasks to Archive" tags "-REFILE/"
                     ((org-agenda-overriding-header "Tasks to Archive")
                      (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                      (org-tags-match-list-sublevels nil))))))

      ;; Keep tasks with dates on the global todo lists
      (setq org-agenda-todo-ignore-with-date t
            ;; Keep tasks with deadlines on the global todo lists
            org-agenda-todo-ignore-deadlines t
            ;; Keep tasks with scheduled dates on the global todo lists
            org-agenda-todo-ignore-scheduled t
            ;; Keep tasks with timestamps on the global todo lists
            org-agenda-todo-ignore-timestamp t
            ;; Remove completed deadline tasks from the agenda view
            org-agenda-skip-deadline-if-done t
            ;; Remove completed scheduled tasks from the agenda view
            org-agenda-skip-scheduled-if-done t
            ;; Remove completed items from search results
            org-agenda-skip-timestamp-if-done t)

                                        ; position the habit graph on the agenda to the right of the default
      ;; (setq org-habit-show-habits nil
      ;;       org-habit-graph-column 40
      ;;       org-habit-show-habits-only-for-today t
      ;;       org-habit-following-days 2
      ;;       org-habit-preceding-days 42
      ;;       org-habit-show-all-today t)

      ;; Use sticky agenda's so they persist
      (setq org-agenda-sticky t)

      ;; Archiving
      (setq org-archive-mark-done nil)
      (setq org-archive-location "%s_archive::* Archived Tasks"))))

(defun spacemacs/init-ace-jump-mode ()
  (use-package ace-jump-mode
    :defer t
    :init
    (progn
      (add-hook 'ace-jump-mode-before-jump-hook 'evil-set-jump)
      (add-hook 'ace-jump-mode-end-hook 'golden-ratio)
      (spacemacs/set-leader-keys "SPC" 'evil-ace-jump-word-mode)
      (spacemacs/set-leader-keys "l" 'evil-ace-jump-line-mode))
    :config
    (progn
      (setq ace-jump-mode-scope 'window)
      (spacemacs/set-leader-keys "`" 'ace-jump-mode-pop-mark))))

(defun luxbock/init-imenu-anywhere ()
  (use-package imenu-anywhere
    :config
    (spacemacs/set-leader-keys "hi" 'helm-imenu-anywhere)))

(defun luxbock/init-clojure-mode-extra-font-locking ()
  (use-package clojure-mode-extra-font-locking))

(defun luxbock/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :config
    (bind-keys ("M-%"   . vr/query-replace)
               ("C-M-%" . vr/replace))))

(defun luxbock/init-git-auto-commit-mode ()
  (use-package git-auto-commit-mode))

(defun luxbock/init-drag-stuff ()
  (use-package drag-stuff
    :config
    (progn
      (setq drag-stuff-except-modes '(org-mode emacs-lisp-mode clojure-mode))
      (drag-stuff-global-mode t))))

(defun luxbock/init-dired-filter ()
  (use-package dired-filter))

(defun luxbock/init-dired-subtree ()
  (use-package dired-subtree
    :config
    (bind-keys :map dired-mode-map
               :prefix "C-,"
               :prefix-map dired-subtree-map
               :prefix-docstring "Dired subtree map."
      ("C-i" . dired-subtree-insert)
      ("C-/" . dired-subtree-apply-filter)
      ("C-k" . dired-subtree-remove)
      ("C-n" . dired-subtree-next-sibling)
      ("C-p" . dired-subtree-previous-sibling)
      ("C-u" . dired-subtree-up)
      ("C-d" . dired-subtree-down)
      ("C-a" . dired-subtree-beginning)
      ("C-e" . dired-subtree-end)
      ("m" . dired-subtree-mark-subtree)
      ("u" . dired-subtree-unmark-subtree)
      ("C-o C-f" . dired-subtree-only-this-file)
      ("C-o C-d" . dired-subtree-only-this-directory))))

(defun luxbock/init-tiny ()
  (use-package tiny
    :config
    (global-set-key (kbd "C-c t") 'tiny-expand)))

(defun luxbock/init-htmlize ()
  (use-package htmlize))

(defun luxbock/init-multiple-cursors ()
  (use-package multiple-cursors
    :diminish "mc"
    :config
    (progn
      (bind-keys
       ("C->"           . mc/mark-next-word-like-this)
       ("C-<"           . mc/mark-previous-word-like-this)
       ("C-*"           . mc/mark-all-words-like-this)
       ("C-&"           . mc/mark-all-like-this-dwim)
       ("C-$"           . mc/mark-all-like-this-in-defun)
       ("C-S-<mouse-1>" . mc/add-cursor-on-click)
       ("C-%"           . mc/mark-all-in-region))

      ;; fix for working with evil-states
      ;; https://github.com/magnars/multiple-cursors.el/issues/19
      (defvar my-mc-evil-previous-state nil)

      (defun my-mc-evil-switch-to-emacs-state ()
        (when (and (bound-and-true-p evil-mode)
                   (not (eq evil-state 'emacs)))
          (setq my-mc-evil-previous-state evil-state)
          (evil-emacs-state)))

      (defun my-mc-evil-back-to-previous-state ()
        (when my-mc-evil-previous-state
          (unwind-protect
              (case my-mc-evil-previous-state
                ((normal visual insert) (evil-force-normal-state))
                (t (message "Don't know how to handle previous state: %S"
                            my-mc-evil-previous-state)))
            (setq my-mc-evil-previous-state nil))))

      (add-hook 'multiple-cursors-mode-enabled-hook
                'my-mc-evil-switch-to-emacs-state)
      (add-hook 'multiple-cursors-mode-disabled-hook
                'my-mc-evil-back-to-previous-state)
      )))

(defun luxbock/init-rainbow-mode ()
  (use-package rainbow-mode))

(defun luxbock/init-paxedit ()
  (use-package paxedit))

(defun luxbock/init-ox-gfm ()
  (use-package org-gfm))

(defun luxbock/init-typed-clojure-mode ()
  (use-package typed-clojure-mode))

(defun luxbock/init-edn ()
  (use-package edn))

(defun luxbock/init-scratch ()
  (use-package scratch
   :commands scratch
   :init
   (spacemacs/set-leader-keys "os" 'scratch)))

(defun luxbock/init-feature-mode ()
  (use-package feature-mode
    :mode ("\\.feature$" . feature-mode)
    :diminish "fm"))

(defun luxbock/init-evil-cleverparens ()
  (use-package evil-cleverparens
    :config
    (progn
      (setq evil-cleverparens-use-additional-bindings t)
      (setq evil-cleverparens-swap-move-by-word-and-symbol t)
      (dolist (this-hook '(cider-repl-mode-hook
                           cider-mode-hook
                           emacs-lisp-mode-hook))
        (add-hook this-hook #'evil-cleverparens-mode)))))
