(setq scroll-margin 2)
(setq kill-ring-max 200)

(add-hook 'prog-mode-hook 'linum-mode)

;; Reverses the beheavior of spacemacs/config.el
(setq initial-major-mode 'emacs-lisp-mode)

;; From http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

