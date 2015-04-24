(defvar luxbock-post-extensions
  '(
    leuven-dark-theme
    leuven-theme
    evil-paxedit
    align-let
    org-projectile
    evil-cleverparens
    ))

(defun luxbock/init-leuven-dark-theme ()
  (add-to-list 'custom-theme-load-path "~/.emacs.d/private/luxbock/extensions/leuven-dark-theme")
  )

(defun luxbock/init-leuven-theme ()
  (add-to-list 'custom-theme-load-path "~/.emacs.d/private/luxbock/extensions/leuven-theme")
  )

(defun luxbock/init-evil-paxedit-mode ()
  (require 'evil-paxedit)
  )

(defun luxbock/init-align-let ()
  (require 'align-let)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode "ma" 'align-let))

(defun luxbock/init-org-projectile ()
  (require 'org-projectile)
  (setq org-projectile:projects-file "~/org/projects.org")
  (setq luxbock/org-projectile-todo-template "* TODO %? @%A\n%U\n")
  (setq luxbock/org-projectile-note-template "* %? :NOTE:\n%U\n")

  (defun luxbock/org-projectile-capture-for-current-project (arg)
    (interactive "P")
    (let ((template (if arg luxbock/org-projectile-note-template
                      luxbock/org-projectile-todo-template)))
      (org-projectile:capture-for-current-project template)))

  (evil-leader/set-key
    "op" 'luxbock/org-projectile-capture-for-current-project))

(defun luxbock/init-evil-cleverparens ()
  (setq evil-cleverparens-swap-move-by-word-and-symbol t)
  (require 'evil-cleverparens)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (add-hook 'cider-mode-hook #'evil-cleverparens-mode)
  (add-hook 'cider-repl-mode-hook #'evil-cleverparens-mode)
  (add-hook 'cider-clojure-interaction-mode-hook #'evil-cleverparens-mode))
