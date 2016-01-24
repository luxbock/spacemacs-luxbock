;; From https://github.com/cofi/dotfiles/blob/master/emacs.d/

(setq min-window-width (* 2 81))

(defun lux/smart-split ()
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

:;; Emacs Lisp

(defun lux/eval-and-replace ()
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

(defun lux/copy-last-message (p)
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
        (set-window-start other-window this-start)
        (select-window other-window)))))

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

(defun apropos-face (pattern)
  (interactive
   (list (apropos-read-pattern "symbol")))
  (apropos-parse-pattern pattern)
  (apropos-symbols-internal (apropos-internal apropos-regexp 'facep) t))

(defun lux/clj-eval-with-args (arg)
  (interactive "P")
  (save-excursion
    (sp-beginning-of-sexp)
    (let ((sap (symbol-at-point)))
      (cider-interactive-eval
       (format "(%s %s)" sap
               (read-string (format "Call \"%s\" with: " sap)))))))

(defun luxbock/show-atreus-layout ()
  (interactive)
  (find-file "/Users/Olli/spacemacs-luxbock/atreus/layout-all.svg"))


(defvar luxbock/command-is-meta nil)

(defun lux/swap-command-key ()
  "Toggles the functionality of the Command and Option keys on a Mac."
  (interactive)
  (if luxbock/command-is-meta
      (setq mac-command-modifier 'super
            mac-option-modifier 'meta
            luxbock/command-is-meta nil)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super
          luxbock/command-is-meta t))
  (message
   (format "Meta is %s"
           (if luxbock/command-is-meta "Command" "Alt"))))

(defun lux/insert-space-in-front ()
  (interactive)
  (insert " ")
  (backward-char))

;;; Eshell alises
(defun eshell/clear ()           (recenter 0))
(defun eshell/ll    (&rest args) (apply 'eshell/ls args))
(defun eshell/d     (dir)        (dired dir))
(defun eshell/ff    (file)       (find-file file))
(defun eshell/fx    (file)       (find-file-other-window file))

;; Calling JSON end-points
(defun lux/get-json (url)
  (let ((url-request-method "GET"))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let ((json-object-type 'plist)
            (json-key-type 'keyword)
            (json-array-type 'vector))
        (json-read)))))

(defun lux/forex (from to)
  (let ((result (lux/get-json (concat "https://api.fixer.io/latest?base=" from))))
    (plist-get (plist-get result :rates)
               (intern (concat ":" to)))))

(defun lux/btc-price ()
  "Calls the Bitstamp API to retrieve the latest Bitcoin price,
  returning it as a string."
  (let ((result (lux/get-json "https://www.bitstamp.net/api/ticker/")))
    (string-to-number (plist-get result :last))))

;;; ERC randomness

(defun lux/-users-for-channel (channel-buffer)
  (with-current-buffer channel-buffer
    (let ((nick (erc-current-nick)))
      (-remove (lambda (user) (member user `(,nick "chanserv")))
               (hash-table-keys erc-channel-users)))))

(defun lux/pick-two (list)
  (-mapcat (lambda (sublist)
             (-map (lambda (x) (list (car sublist) x))
                   (cdr sublist)))
           (-iterate 'cdr list (length list))))

(defun lux/erc-shared-users (&rest channels)
  (let ((chan->users (make-hash-table))
        (union->users (make-hash-table))
        return)
    (dolist (c channels)
      (puthash c (lux/-users-for-channel c) chan->users))
    (dolist (channels (lux/pick-two channels))
      (let ((shared
             (reduce (lambda (acc users)
                       (intersection acc users :test 'string-equal))
                     (-map (lambda (ch)
                             (gethash ch chan->users))
                           channels))))
        (when shared
          (setq return (cons (list channels shared) return)))))
    return))

;;; Indentation
(defun lux/align-dwim ()
  (interactive)
  (let ((n-spaces (lambda (n) (insert (apply 'concat (-repeat n " ")))))))
  (if (evil-visual-state-p)
      (destructuring-bind (beg end &rest) (evil-visual-range)
        (let ((regx (read-regexp "Align regexp: "))
              (lc   (count-lines beg end))
              matches)
          (save-excursion
            (goto-char beg)
            (dotimes (i lc)
              (let ((begl (beginning-of-line)))
                (when (search-forward-regexp regx (point-at-eol) t)
                  (goto-char (match-beginning 0))
                  (setq matches (cons (current-column) matches))
                  (forward-line))))
            (when matches
              (let ((furthest (-max matches)))
                (dolist (m matches)
                  (when (< m furthest)
                    (goto-char m)
                    (funcall 'n-spaces (- furthest m)))))))))
    (let* ((col (current-column))
           (last-char (save-excursion
                        (when (evil-insert-state-p)
                          (backward-char))
                        (thing-at-point 'char)))
           (p-col (save-excursion
                    (forward-line -1)
                    (evil-forward-char col)
                    (when (search-forward last-char (point-at-eol) t)
                      (current-column)))))
      (when p-col
        (let ((offset (- p-col col)))
          (if (evil-normal-state-p)
              (funcall 'n-spaces (1- offset))
            (backward-char)
            (funcall 'n-spaces offset)
            (forward-char)))))))

;; Newlines
(defun lux/newline-below-dont-move (c)
  (interactive "p")
  (save-excursion
    (forward-line)
    (dotimes (_ c) (insert "\n"))))

(defun lux/newline-above-dont-move (c)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (dotimes (_ c) (insert "\n"))))

(defun lux/newline-above-and-indent ()
  (interactive)
  (beginning-of-line)
  (insert "\n")
  (forward-line -1)
  (indent-according-to-mode))
