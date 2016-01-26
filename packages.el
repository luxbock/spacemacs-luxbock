(setq luxbock-packages
      '(
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
        evil-cleverparens
        (align-let :location local)
        (dactyl-mode :location local)))

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

;; (defun luxbock/init-ox-gfm ()
;;   (use-package org-gfm))

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

(defun luxbock/init-align-let ()
  (use-package align-let
    :init
   (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "ma" 'align-let)))

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

(defun luxbock/init-dactyl-mode ()
  (use-package dactyl-mode
    :mode ("\\pentadactylrc$" . dactyl-mode)))
