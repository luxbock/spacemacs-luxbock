(setq luxbock-post-extensions
  '(
    material-theme
    org-projectile
    dactyl-mode
    ))

(defun luxbock/init-leuven-dark-theme ()
  (add-to-list 'custom-theme-load-path "~/.emacs.d/private/luxbock/extensions/leuven-dark-theme"))

(defun luxbock/init-evil-paxedit-mode ()
  (require 'evil-paxedit))

(defun luxbock/init-align-let ()
  (require 'align-let)
  (spacemacs/set-leader-keys 'emacs-lisp-mode "ma" 'align-let))

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

  (spacemacs/set-leader-keys
    "op" 'luxbock/org-projectile-capture-for-current-project))

(defun luxbock/init-material-theme ()
  (add-to-list 'custom-theme-load-path "~/spacemacs-luxbock/extensions/emacs-material-theme"))

(defun luxbock/init-dactyl-mode ()
  (use-package dactyl-mode
    :mode ("\\pentadactylrc$" . dactyl-mode)))
