(defvar luxbock-packages
  '(evil
    helm
    org
    ace-jump-mode
    imenu-anywhere
    clojure-mode-extra-font-locking
    visual-regexp-steroids
    dired-subtree
    dired-filter
    yasnippet
    yagist
    git-auto-commit-mode
    drag-stuff
    tiny
    htmlize
    multiple-cursors
    rainbow-mode
    highlight-indentation
    paxedit
    linum-relative
    powerline
    helm-swoop
    ;; swiper-helm
    outorg
    outshine
    engine-mode
    ))


;; Evil key-binding helpers
(defun set-in-all-evil-states (key def &optional maps)
  (unless maps
    (setq maps (list evil-normal-state-map
                     evil-visual-state-map
                     evilinsert-state-map
                     evil-emacs-state-map
                     evil-motion-state-map)))
  (while maps
    (define-key (pop maps) key def)))


(defun set-in-all-evil-states-but-insert (key def)
  (set-in-all-evil-states key def
                          (list evil-normal-state-map
                                evil-visual-state-map
                                evil-motion-state-map)))

(defun luxbock/init-evil ()
  (use-package evil
    :init
    (progn

      (defun spacemacs/state-color-face (state)
        "Return the symbol of the face for the given STATE."
        (intern (format "spacemacs-%s-face" (symbol-name state))))

      (defun spacemacs/defface-state-color (state color)
        "Define a face for the given STATE and background COLOR."
        (eval `(defface ,(spacemacs/state-color-face state) '((t ()))
                 ,(format "%s state face." (symbol-name state))
                 :group 'spacemacs))
        (set-face-attribute (spacemacs/state-color-face state) nil
                            :background color
                            :foreground (face-background 'mode-line)
                            :box (face-attribute 'mode-line :box)
                            :inherit 'mode-line))

      (defun spacemacs/state-color (state)
        "Return the color string associated to STATE."
        (face-background (spacemacs/state-color-face state)))

      (defun spacemacs/current-state-color ()
        "Return the color string associated to the current state."
        (face-background (spacemacs/state-color-face evil-state)))

      (defun spacemacs/state-face (state)
        "Return the face associated to the STATE."
        (spacemacs/state-color-face state))

      (defun spacemacs/current-state-face ()
        "Return the face associated to the current state."
        (let ((state (if (eq evil-state 'operator)
                         evil-previous-state
                       evil-state)))
          (spacemacs/state-color-face state)))

      (defun spacemacs/set-state-faces ()
        "Define or set the state faces."
        (mapcar (lambda (x) (spacemacs/defface-state-color (car x) (cdr x)))
                '((normal . "DarkGoldenrod2")
                  (insert . "chartreuse3")
                  (emacs  . "SkyBlue2")
                  (visual . "gray")
                  (motion . "plum3")
                  (lisp   . "HotPink1"))))
      (spacemacs/set-state-faces)

      (defun set-default-evil-emacs-state-cursor ()
        (setq evil-emacs-state-cursor `(,(spacemacs/state-color 'emacs) box)))

      (defun set-default-evil-normal-state-cursor ()
        (setq evil-normal-state-cursor `(,(spacemacs/state-color 'normal) box)))

      (defun set-default-evil-insert-state-cursor ()
        (setq evil-insert-state-cursor `(,(spacemacs/state-color 'insert) (bar . 2))))

      (defun set-default-evil-visual-state-cursor ()
        (setq evil-visual-state-cursor `(,(spacemacs/state-color 'visual) (hbar . 2))))

      (defun set-default-evil-motion-state-cursor ()
        (setq evil-motion-state-cursor `(,(spacemacs/state-color 'motion) box)))

      (defun set-default-evil-lisp-state-cursor ()
        (setq evil-lisp-state-cursor `(,(spacemacs/state-color 'lisp) box)))

      (defun evil-insert-state-cursor-hide ()
        (setq evil-insert-state-cursor `(,(spacemacs/state-color 'insert) (hbar . 0))))

      (set-default-evil-emacs-state-cursor)
      (set-default-evil-normal-state-cursor)
      (set-default-evil-insert-state-cursor)
      (set-default-evil-visual-state-cursor)
      (set-default-evil-motion-state-cursor)
      (set-default-evil-lisp-state-cursor)
      (evil-mode 1))

    :config
    (progn
      (setq evil-cross-lines nil
            evil-want-visual-char-semi-exclusive t
            evil-want-fine-undo nil
            evil-move-cursor-back nil
            evil-insert-state-cursor nil)

      (setq-default evil-symbol-word-search t)

      (add-hook 'org-capture-mode-hook #'evil-insert-state)

      (defun luxbock/kmacro-start-or-stop ()
        (interactive)
        (if defining-kbd-macro
            (kmacro-end-macro 1)
          (kmacro-start-macro 1)))

      ;; Keys
      (bind-keys :map evil-normal-state-map
                 ;; The opposite of what Vim does, but I like it better
                 ("'"     . evil-goto-mark)
                 ("`"     . evil-goto-mark-line)
                 ("gt"    . goto-char)
                 ("gl"    . goto-line)
                 ("\\"    . evil-repeat-find-char-reverse)
                 ("gj"    . evil-next-line)
                 ("gk"    . evil-previous-line)
                 ("j"     . evil-next-visual-line)
                 ("k"     . evil-previous-visual-line)
                 ("Q"     . luxbock/kmacro-start-or-stop))

      (bind-keys :map evil-motion-state-map
                 ("_"           . evil-first-non-blank)
                 ("j"           . evil-next-visual-line)
                 ("k"           . evil-previous-visual-line))

      (bind-keys :map evil-insert-state-map
                 ("C-k"        . nil)
                 ("C-y"        . nil)
                 ("C-n"        . nil)
                 ("C-d"        . nil)
                 ("C-w"        . backward-kill-word)
                 ("C-x C-n"    . evil-complete-next-line)
                 ("C-x C-p"    . evil-complete-previous-line)
                 ("C-SPC"      . company-complete-common))

      ;; Windows
      (bind-keys :map evil-emacs-state-map ("C-w" . evil-window-map))
      (bind-keys :prefix "C-w"
           :prefix-map evil-window-map
           ("C-w"        . helm-mini)
           ("0"          . delete-window)
           ("1"          . delete-other-windows)
           ("2"          . split-window-below)
           ("3"          . split-window-right)
           ("h"          . evil-window-left)
           ("H"          . scroll-left)
           ("L"          . scroll-right)
           ("l"          . evil-window-right)
           ("k"          . evil-window-up)
           ("j"          . evil-window-down)
           ("u"          . winner-undo)
           ("r"          . winner-redo)
           ("n"          . evil-window-next)
           ("m"          . evil-window-prev)
           ("w"          . op/toggle-previous-buffer)
           ("N"          . evil-window-prev)
           ("J"          . scroll-other-window)
           ("K"          . scroll-other-window-down)
           ("c"          . evil-window-new)
           ("d"          . toggle-current-window-dedication)
           ("b"          . switch-to-buffer)
           ("B"          . switch-to-buffer-other-window)
           ("o"          . other-window)
           ("D"          . kill-buffer-and-window)
           ("RET"        . enlarge-window)
           ("C-<return>" . enlarge-window)
           ;; Swapping
           ("M-h"        . swap-with-left)
           ("M-j"        . swap-with-down)
           ("M-k"        . swap-with-up)
           ("M-l"        . swap-with-right)
           ("SPC"        . swap-window)
           ;; Splitting
           ("|"          . evil-window-vsplit)
           ("\\"         . evil-window-split)
           ("s"          . cofi/smart-split)
           ;; Swapping
           ("M-h"        . swap-with-left)
           ("M-l"        . swap-with-right)
           ("M-j"        . swap-with-down)
           ("M-k"        . swap-with-up)
           ;; Adjusting height
           ("="          . balance-windows)
           ("+"          . evil-window-increase-height)
           ("-"          . evil-window-decrease-height)
           ("a"          . evil-window-decrease-height)
           ("q"          . evil-window-increase-height)
           ("C-q"        . evil-window-increase-height)
           ("C-a"        . evil-window-decrease-height)
           ("e"          . op/window-shortcut-map)
           ("C-e"        . op/window-shortcut-map))

      ;; Scrolling

      (defun scroll-up-by-ten ()
        (interactive)
        (evil-scroll-up 10))

      (defun scroll-down-by-ten ()
        (interactive)
        (evil-scroll-down 10))

      (set-in-all-evil-states-but-insert (kbd "C-u") 'scroll-up-by-ten)
      (set-in-all-evil-states-but-insert (kbd "C-d") 'scroll-down-by-ten)
      (define-key evil-insert-state-map  (kbd "C-v") 'scroll-down-by-ten)
      (define-key evil-insert-state-map  (kbd "M-v") 'scroll-up-by-ten)
      (define-key evil-emacs-state-map   (kbd "C-v") 'scroll-down-by-ten)
      (define-key evil-emacs-state-map   (kbd "M-v") 'scroll-up-by-ten)

      (defadvice evil-goto-mark (before goto-mark-evil-jump activate)
        (evil-set-jump))

      (defadvice evil-goto-mark-line (before goto-mark-line-evil-jump activate)
        (evil-set-jump))

      ;; evil ex-command key
      (define-key evil-motion-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      ;; Make evil-mode up/down operate in screen lines instead of logical lines
      (define-key evil-normal-state-map "j" 'evil-next-visual-line)
      (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
      ;; Make the current definition and/or comment visible.
      (define-key evil-normal-state-map "zf" 'reposition-window)
      ;; quick navigation
      (define-key evil-normal-state-map (kbd "L")
        (lambda () (interactive)
          (evil-window-bottom)
          (let ((recenter-redisplay nil))
            (recenter nil))))
      (define-key evil-normal-state-map (kbd "H")
        (lambda () (interactive)
          (evil-window-top)
          (let ((recenter-redisplay nil))
            (recenter nil))))
      (evil-leader/set-key "re" 'evil-show-registers)
      ;; define text objects
      (defmacro spacemacs|define-and-bind-text-object (key name start-regex end-regex)
        (let ((inner-name (make-symbol (concat "evil-inner-" name)))
              (outer-name (make-symbol (concat "evil-outer-" name))))
          `(progn
             (evil-define-text-object ,inner-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count nil))
             (evil-define-text-object ,outer-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count t))
             (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
             (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
      ;; between dollars sign:
      (spacemacs|define-and-bind-text-object "$" "dollar" "\\$" "\\$")
      ;; between pipe characters:
      (spacemacs|define-and-bind-text-object "|" "bar" "|" "|")
      ;; between percent signs:
      (spacemacs|define-and-bind-text-object "%" "percent" "%" "%")

      ;; add star block
      (spacemacs|define-and-bind-text-object "*" "star-block" "/*" "*/")
      ;; add slash block
      (spacemacs|define-and-bind-text-object "/" "slash-block" "//" "//")

      ;; support smart 1parens-strict-mode
      (if (ht-contains? configuration-layer-all-packages 'smartparens)
          (defadvice evil-delete-backward-char-and-join
              (around spacemacs/evil-delete-backward-char-and-join activate)
            (if smartparens-strict-mode
                (call-interactively 'sp-backward-delete-char)
              ad-do-it))))))

(defun luxbock/init-helm ()
  (use-package helm
    :defer t
    :init
    (progn
      (setq helm-split-window-in-side-p nil
            helm-bookmark-show-location t
            helm-buffers-fuzzy-matching t
            helm-always-two-windows     t
            helm-split-window-in-side-p t)

      (evil-leader/set-key
        dotspacemacs-command-key 'helm-M-x
        "bs"  'helm-mini
        "ol"  'helm-colors
        "sl"  'helm-semantic-or-imenu
        "hb"  'helm-bookmarks
        "ho"  'helm-occur
        "hl"  'helm-resume
        "ry"  'helm-show-kill-ring
        "rr"  'helm-register
        "rm"  'helm-all-mark-rings
        "fr"  'helm-recentf
        "fl"  'helm-locate
        "<f1>" 'helm-apropos)

      (when dotspacemacs-helm-micro-state
        (defcustom spacemacs-helm-navigation-micro-state-color
          (face-attribute 'error :foreground)
          "Background color of helm header when helm micro-state is activated."
          :type 'color
          :group 'spacemacs)))

    :config
    (progn
      (helm-mode +1)

      ;; alter helm-bookmark key bindings to be simpler
      (defun simpler-helm-bookmark-keybindings ()
        (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
        (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
        (define-key helm-bookmark-map (kbd "C-f") 'helm-bookmark-toggle-filename)
        (define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
        (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))

      (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)

      ;; helm navigation on hjkl
      (define-key helm-map (kbd "C-j") 'helm-next-line)
      (define-key helm-map (kbd "C-k") 'helm-previous-line)
      (define-key helm-map (kbd "C-h") 'helm-next-source)
      (define-key helm-map (kbd "C-l") 'helm-previous-source)
      (define-key helm-map (kbd "C-w") 'subword-backward-kill)

      ;; eshell
      (defun spacemacs/helm-eshell-history ()
        "Correctly revert to insert state after selection."
        (interactive)
        (helm-eshell-history)
        (evil-insert-state))

      (defun spacemacs/helm-shell-history ()
        "Correctly revert to insert state after selection."
        (interactive)
        (helm-comint-input-ring)
        (evil-insert-state))

      (defun spacemacs/init-helm-eshell ()
        "Initialize helm-eshell."
        ;; this is buggy for now
        ;; (define-key eshell-mode-map (kbd "<tab>") 'helm-esh-pcomplete)
        (evil-leader/set-key-for-mode 'eshell-mode "mH" 'spacemacs/helm-eshell-history))

      (add-hook 'eshell-mode-hook 'spacemacs/init-helm-eshell)
      (evil-leader/set-key-for-mode 'shell-mode "mH" 'spacemacs/helm-shell-history)

      (when dotspacemacs-helm-micro-state
        (defun spacemacs//on-enter-helm-navigation-micro-state ()
          "Initialization of helm micro-state."
          (set-face-attribute
           'helm-header nil
           :background spacemacs-helm-navigation-micro-state-color)
          ;; bind actions on numbers starting from 1 which executes action 0
          (dotimes (n 10)
            (define-key helm-map (number-to-string n)
              `(lambda () (interactive) (helm-select-nth-action
                                         ,(% (+ n 9) 10))))))

        (defun spacemacs//on-exit-helm-navigation-micro-state ()
          "Action to perform when exiting helm micor-state."
          ;; restore helm key map
          (dotimes (n 10) (define-key helm-map (number-to-string n) nil))
          ;; restore faces
          (set-face-attribute
           'helm-header nil
           :background (face-attribute 'header-line :background)))

        (spacemacs|define-micro-state helm-navigation
          :on-enter (spacemacs//on-enter-helm-navigation-micro-state)
          :on-exit  (spacemacs//on-exit-helm-navigation-micro-state)
          :bindings
          ("C-c" nil :exit t)
          ("?" helm-help)
          ("a" helm-select-action)
          ("g" helm-beginning-of-buffer)
          ("G" helm-end-of-buffer)
          ("h" helm-previous-source)
          ("j" helm-next-line)
          ("k" helm-previous-line)
          ("l" helm-next-source)
          ("r" helm-select-action :exit t)
          ("t" helm-toggle-visible-mark)
          ("T" helm-toggle-all-marks)
          ("<tab>" helm-execute-persistent-action)
          ("v" helm-execute-persistent-action)))

      (global-set-key (kbd "C-x C-f") 'helm-find-files)
      (global-set-key (kbd "M-x") 'helm-M-x)

      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-SPC") 'helm-execute-persistent-action)

      (eval-after-load "helm-mode" ; required
        '(spacemacs|hide-lighter helm-mode)))))

;; (defun luxbock/init-org-projectile ()
;;   (use-package org-projectile
;;     ;; :bind ("C-c n p" . org-projectile:template-or-project)
;;     :init
;;     (progn
;;       (setq org-projectile:projects-file "~/org/projects.org")
;;       (setq org-projectile:capture-template "* TODO %?\n%U\n")
;;       (add-to-list 'org-capture-templates
;;                    (org-projectile:project-todo-entry
;;                     "p" "* TODO %?\n%U\n" "Project Todo"))
;;       (add-to-list 'org-capture-templates
;;                    (org-projectile:project-todo-entry
;;                     "N" "* %? :NOTE:\n%U\n" "Project Note"))
;;       )
;;     :ensure t))

(defun luxbock/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :defer t
    :init
    (progn
      (setq org-log-done t)
      (add-hook 'org-mode-hook 'org-indent-mode)
      (evil-leader/set-key-for-mode 'org-mode
        "ot" 'org-capture
        "oa" 'org-agenda
        "mk" 'outline-previous-visible-heading
        "mj" 'outline-next-visible-heading
        "mh" 'outline-up-heading
        "my" 'op/org-extract-link
        "mc" 'org-capture
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mi" 'org-clock-in
        "mo" 'org-clock-out
        "mm" 'org-ctrl-c-ctrl-c
        "mr" 'org-refile
        "ms" 'org-schedule
        "mt" 'org-todo)

      (eval-after-load 'evil-org
        ;; move the leader bindings to `m` prefix to be consistent with
        ;; the rest of spacemacs bindings
        '(evil-leader/set-key-for-mode 'org-mode
           "ma" 'org-agenda
           "mA" 'org-archive-subtree
           "mC" 'evil-org-recompute-clocks
           "ml" 'evil-org-open-links
           "mt" 'org-show-todo-tree)))
    :config
    (progn
      (require 'org-install)
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

      (evil-leader/set-key-for-mode 'org-mode "oi" 'bh/insert-inactive-timestamp)
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
               (file+datetree "~/org/journal.org")
               "* %? :JOURNAL:\n")
              ("s" "Daily Synopsis"
               entry
               (file+datetree "~/org/journal.org")
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
                                    ("STYLE_ALL" . "habit")))

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
      (setq org-habit-graph-column 40
            org-habit-show-habits-only-for-today t
            org-habit-following-days 2
            org-habit-preceding-days 42
            org-habit-show-all-today t)

      ;; Use sticky agenda's so they persist
      (setq org-agenda-sticky t)

      ;; Archiving
      (setq org-archive-mark-done nil)
      (setq org-archive-location "%s_archive::* Archived Tasks")

      )))

(defun spacemacs/init-ace-jump-mode ()
  (use-package ace-jump-mode
    :defer t
    :init
    (progn
      (add-hook 'ace-jump-mode-before-jump-hook 'evil-set-jump)
      (add-hook 'ace-jump-mode-end-hook 'golden-ratio)
      (evil-leader/set-key "SPC" 'evil-ace-jump-word-mode)
      (evil-leader/set-key "l" 'evil-ace-jump-line-mode))
    :config
    (progn
      (setq ace-jump-mode-scope 'window)
      (evil-leader/set-key "`" 'ace-jump-mode-pop-mark))))

;; (defun luxbock/init-rcirc ()
;;   (use-package rcirc
;;     :commands irc
;;     :init
;;     (progn
;;       (add-to-hook 'rcirc-mode-hook '(rcirc-track-minor-mode
;;                                       rcirc-omit-mode
;;                                       ;; rcirc-reconnect-mode
;;                                       flyspell-mode))
;;       (setq evil-normal-state-modes
;;             (cons 'rcirc-mode evil-normal-state-modes)))
;;     :config
;;     (progn
;;       (setq rcirc-fill-column 80
;;             rcirc-buffer-maximum-lines 2048
;;             rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
;;             rcirc-omit-threshold 20
;;             rcirc-default-nick "luxbock"
;;             rcirc-log-flag t
;;             rcirc-log-directory "~/.emacs.d/private/rcirc-log/"
;;             rcirc-auth-info '(("freenode" nickserv "luxbock" "Om3napuu"))
;;             rcirc-server-alist
;;             '(("b3bai.com" :nick "luxbock" :password "luxbock/freenode:Om3napuu" :full-name "Olli")
;;               ("b3bai.com" :nick "luxbock" :password "luxbock/EFnet:Om3napuu" :full-name "Olli")))
;;       (require 'rcirc-color)
;;       (let ((dir (configuration-layer/get-layer-property 'spacemacs :ext-dir)))
;;         (require 'rcirc-reconnect
;;                  (concat dir "rcirc-reconnect/rcirc-reconnect.el")))
;;       ;; identify info are stored in a separate location, skip errors
;;       ;; if the feature cannot be found.
;;       (require 'pinit-rcirc nil 'noerror)
;;       (define-key rcirc-mode-map (kbd "M-j") 'rcirc-insert-prev-input)
;;       (define-key rcirc-mode-map (kbd "M-k") 'rcirc-insert-next-input)
;;       (evil-define-key 'insert rcirc-mode-map (kbd "C-k") 'kill-line))))

(defun luxbock/init-imenu-anywhere ()
  (use-package imenu-anywhere
    :config
    (evil-leader/set-key "hi" 'helm-imenu-anywhere)))

(defun luxbock/init-clojure-mode-extra-font-locking ()
  (use-package clojure-mode-extra-font-locking))

(defun luxbock/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :config
    (bind-keys ("M-%"   . vr/query-replace)
               ("C-M-%" . vr/replace))))

(defun luxbock/init-yasnippet ()
  (use-package yasnippet
    :commands (yas-expand yas-new-snippet)
    :config
    (progn
      (setq yas/root-directory "~/.emacs.d/private/snippets/")
      (yas/load-directory yas/root-directory))))

(defun luxbock/init-yagist ()
  (use-package yagist))

(defun luxbock/init-git-auto-commit-mode ()
  (use-package git-auto-commit-mode))

(defun luxbock/init-drag-stuff ()
  (use-package drag-stuff
    :config
    (bind-keys
     ("M-j" . drag-stuff-down)
     ("M-k" . drag-stuff-up)
     ("M-h" . drag-stuff-left)
     ("M-l" . drag-stuff-right))))

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

(defun luxbock/init-highlight-indentation ()
  (use-package highlight-indentation))

(defun luxbock/init-paxedit ()
  (use-package paxedit))

(defun luxbock/init-powerline ()
  (use-package powerline
    :init
    (progn
      ;; Custom format of minor mode lighters, they are separated by a pipe.
      (defpowerline spacemacs-powerline-minor-modes
        (mapconcat (lambda (mm)
                     (propertize
                      mm
                      'mouse-face 'mode-line-highlight
                      'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map
                                     [mode-line down-mouse-1]
                                     (powerline-mouse 'minor 'menu mm))
                                   (define-key map
                                     [mode-line mouse-2]
                                     (powerline-mouse 'minor 'help mm))
                                   (define-key map
                                     [mode-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu mm))
                                   (define-key map
                                     [header-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu mm))
                                   map)))
                   (split-string (format-mode-line minor-mode-alist))
                   (concat (propertize
                            (if dotspacemacs-mode-line-unicode-symbols " " "") 'face face)
                           (unless dotspacemacs-mode-line-unicode-symbols "|"))))

      (defpowerline spacemacs-powerline-new-version
        (propertize
         spacemacs-version-check-lighter
         'mouse-face 'mode-line-highlight
         'help-echo (format "New version %s | Click with mouse-1 to update (Not Yet Implemented)"
                            spacemacs-new-version)
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map
                        [mode-line down-mouse-1]
                        (lambda (event) (interactive "@e") (message "TODO: update"))
                        )
                      map)))

      (defvar spacemacs-mode-line-minor-modesp t
        "If not nil, minor modes lighter are displayed in the mode-line.")
      (defun spacemacs/mode-line-minor-modes-toggle ()
        "Toggle display of minor modes."
        (interactive)
        (if spacemacs-mode-line-minor-modesp
            (setq spacemacs-mode-line-minor-modesp nil)
          (setq spacemacs-mode-line-minor-modesp t)))
      (evil-leader/set-key "tmm" 'spacemacs/mode-line-minor-modes-toggle)

      (defvar spacemacs-mode-line-new-version-lighterp t
        "If not nil, new version lighter is displayed in the mode-line.")
      (defun spacemacs/mode-line-new-version-lighter-toggle ()
        "Toggle display of new version lighter."
        (interactive)
        (if spacemacs-mode-line-new-version-lighterp
            (setq spacemacs-mode-line-new-version-lighterp nil)
          (setq spacemacs-mode-line-new-version-lighterp t)))
      (evil-leader/set-key "tmv" 'spacemacs/mode-line-new-version-lighter-toggle)

      (defvar spacemacs-mode-line-org-clock-current-taskp t
        "If not nil, the currently clocked org-mode task will be
displayed in the mode-line.")
      (defvar spacemacs-mode-line-org-clock-format-function
        'org-clock-get-clock-string
        "Function used to render the currently clocked org-mode task.")
      (defun spacemacs/mode-line-org-org-clock-current-task-toggle ()
        (interactive)
        (if spacemacs-mode-line-new-version-lighterp
            (setq spacemacs-mode-line-org-clock-current-taskp nil)
          (setq spacemacs-mode-line-org-clock-current-taskp t)))
      (evil-leader/set-key "tmc" 'spacemacs/mode-line-org-clock-current-task-toggle)

      (setq-default powerline-default-separator 'wave)

      (defun spacemacs/mode-line-prepare-left ()
        (let* ((active (powerline-selected-window-active))
               (line-face (if active 'mode-line 'mode-line-inactive))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (state-face (if active (spacemacs/current-state-face) face2))
               (window-numberingp (and (boundp 'window-numbering-mode)
                                       (symbol-value window-numbering-mode)))
               (anzup (and (boundp 'anzu--state) anzu--state))
               (flycheckp (and (boundp 'flycheck-mode)
                               (symbol-value flycheck-mode)
                               (or flycheck-current-errors
                                   (eq 'running flycheck-last-status-change))))
               (vc-face (if (or flycheckp spacemacs-mode-line-minor-modesp)
                            face1 line-face))
               (separator-left (intern (format "powerline-%s-%s"
                                               powerline-default-separator
                                               (car powerline-default-separator-dir))))
               (separator-right (intern (format "powerline-%s-%s"
                                                powerline-default-separator
                                                (cdr powerline-default-separator-dir)))))
          (append
           ;; window number
           (if (and window-numberingp (spacemacs/window-number))
               (list (powerline-raw (spacemacs/window-number) state-face))
             (list (powerline-raw (evil-state-property evil-state :tag t) state-face)))
           (if (and active anzup)
               (list (funcall separator-right state-face face1)
                     (powerline-raw (anzu--update-mode-line) face1)
                     (funcall separator-right face1 line-face))
             (list (funcall separator-right state-face line-face)))
           ;; evil state
           ;; (powerline-raw evil-mode-line-tag state-face)
           ;; (funcall separator-right state-face line-face)
           ;; buffer name
           (list
            (powerline-raw "%*" line-face 'l)
            (powerline-buffer-size line-face 'l)
            (powerline-buffer-id line-face 'l)
            (powerline-raw " " line-face)
            ;; major mode
            (funcall separator-right line-face face1)
            (powerline-major-mode face1 'l)
            (powerline-raw " " face1)
            (when active
              (funcall separator-right face1 line-face)))
           ;; flycheck
           (when (and active flycheckp)
             (list (powerline-raw " " line-face)
                   (powerline-raw (spacemacs|custom-flycheck-lighter error)
                                  'spacemacs-mode-line-flycheck-error-face)
                   (powerline-raw (spacemacs|custom-flycheck-lighter warning)
                                  'spacemacs-mode-line-flycheck-warning-face)
                   (powerline-raw (spacemacs|custom-flycheck-lighter info)
                                  'spacemacs-mode-line-flycheck-info-face)))
           ;; separator between flycheck and minor modes
           (when (and active flycheckp spacemacs-mode-line-minor-modesp)
             (list (funcall separator-right line-face face1)
                   (powerline-raw "  " face1)
                   (funcall separator-right face1 line-face)))
           ;; minor modes
           (when (and active spacemacs-mode-line-minor-modesp)
             (list (spacemacs-powerline-minor-modes line-face 'l)
                   (powerline-raw mode-line-process line-face 'l)
                   (powerline-raw " " line-face)))
           ;; version control
           (when (and active (or flycheckp spacemacs-mode-line-minor-modesp))
             (list (funcall separator-right (if vc-face line-face face1) vc-face)))
           (if active
               (list (powerline-vc vc-face)
                     (powerline-raw " " vc-face)
                     (funcall separator-right vc-face face2))
             (list (funcall separator-right face1 face2)))
           ;; org-mode clocked task
           (when (and active
                      (fboundp 'org-clocking-p)
                      spacemacs-mode-line-org-clock-current-taskp
                      (org-clocking-p))
             (list (powerline-raw " " face2)
                   (funcall spacemacs-mode-line-org-clock-format-function)
                   (powerline-raw " " face2))))))

      (defun spacemacs/mode-line-prepare-right ()
        (let* ((active (powerline-selected-window-active))
               (line-face (if active 'mode-line 'mode-line-inactive))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (state-face (if active (spacemacs/current-state-face) face2))
               (nyancatp (and (boundp 'nyan-mode) nyan-mode))
               (batteryp (and (boundp 'fancy-battery-mode)
                              (symbol-value fancy-battery-mode)))
               (battery-face (if batteryp (fancy-battery-powerline-face)))
               (separator-right (intern (format "powerline-%s-%s"
                                                powerline-default-separator
                                                (car powerline-default-separator-dir))))
               (separator-right (intern (format "powerline-%s-%s"
                                                powerline-default-separator
                                                (cdr powerline-default-separator-dir)))))
          (append
           ;; battery
           (if (and active batteryp)
               (list (funcall separator-right face2 battery-face)
                     (powerline-raw (fancy-battery-default-mode-line)
                                    battery-face 'r)
                     (funcall separator-right battery-face face1))
             (list (funcall separator-right face2 face1)))
           (list
            ;; row:column
            (powerline-raw " " face1)
            (powerline-raw
             (concat (format "(%d) " (point))
                     "%l:%2c")
             face1 'r)
            (funcall separator-right face1 line-face)
            (powerline-raw " " line-face))
           (list
            ;; global-mode
            (unless (equal '("") global-mode-string)
              (powerline-raw global-mode-string)
              (powerline-raw " " line-face))
            ;; new version
            (if (and active
                     spacemacs-new-version
                     spacemacs-mode-line-new-version-lighterp)
                (spacemacs-powerline-new-version
                 (spacemacs/get-new-version-lighter-face
                  spacemacs-version spacemacs-new-version) 'r)))
           (when (and active (not nyancatp))
             (let ((progress (format-mode-line "%p")))
               (list
                ;; percentage in the file
                (powerline-raw "%p" line-face 'r)
                ;; display hud
                (powerline-chamfer-left line-face face1)
                (if (string-match "\%" progress)
                    (powerline-hud state-face face1))))))))

      (defun spacemacs/mode-line-prepare ()
        (let* ((active (powerline-selected-window-active))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (lhs (spacemacs/mode-line-prepare-left))
               (rhs (spacemacs/mode-line-prepare-right))
               (nyancatp (and (boundp 'nyan-mode) nyan-mode)))
          (concat (powerline-render lhs)
                  (when (and active nyancatp)
                    (powerline-render (spacemacs/powerline-nyan-cat)))
                  (powerline-fill face2 (powerline-width rhs))
                  (powerline-render rhs))))

      (setq-default mode-line-format
                    '("%e" (:eval (spacemacs/mode-line-prepare))))

      (defun spacemacs//set-powerline-for-startup-buffers ()
        "Set the powerline for buffers created when Emacs starts."
        (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
          (when (get-buffer buffer)
            (with-current-buffer buffer
              (setq-local mode-line-format
                          '("%e" (:eval (spacemacs/mode-line-prepare))))
              (powerline-set-selected-window)
              (powerline-reset)))))
      (add-hook 'after-init-hook
                'spacemacs//set-powerline-for-startup-buffers))))

(defun luxbock/init-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :init
    (setq helm-swoop-split-with-multiple-windows t
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-split-window-function 'helm-default-display-buffer
          helm-swoop-pre-input-function (lambda () ""))
    (evil-leader/set-key
      "sS"    'helm-multi-swoop
      "ss"    'helm-swoop
      "s C-s" 'helm-multi-swoop-all)
    (defadvice helm-swoop (before add-evil-jump activate)
      (when (configuration-layer/package-declaredp 'evil-jumper)
        (evil-set-jump)))))

;; (defun luxbock/init-swiper-helm ()
;;   (use-package swiper-helm
;;     :defer t
;;     :init
;;     (progn
;;       (evil-leader/set-key "ss" 'swiper-helm)
;;       (defadvice swiper-helm (before add-evil-jump activate)
;;         (when (configuration-layer/package-declaredp 'evil-jumper)
;;           (evil-set-jump))))))

(defun luxbock/init-engine-mode ()
  "From: https://github.com/hrs/engine-mode"
  (use-package engine-mode
    :defer t
    :config
    (progn
      (defengine google
        "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
        "SPC")
      (defengine github
        "https://github.com/search?ref=simplesearch&q=%s" "g")
      (defengine stack-overflow
        "https://stackoverflow.com/search?q=%s" "s")
      (defengine twitter
        "https://twitter.com/search?q=%s" "t")
      (defengine wikipedia
        "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
        "w"))))
