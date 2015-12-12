;;; Leader

(spacemacs/declare-prefix "y" "yasnippet")

(spacemacs/set-leader-keys
  "ws" 'cofi/smart-split
  "w C-l" 'swap-with-right
  "w C-h" 'swap-with-left
  "w C-j" 'swap-with-down
  "w C-k" 'swap-with-up

  "fS" 'save-some-buffers

  "nd" 'narrow-or-widen-dwim

  "om" 'op/copy-last-message
  "ow" 'whitespace-cleanup
  "oh" 'mark-whole-buffer
  "or" 'regexp-builder
  "ox" 'luxbock/swap-command-key
  "oa"  'luxbock/switch-to-default-agenda-view
  "ot"  'org-capture
  "wf"  'spacemacs/toggle-fullscreen
  "oci" 'bh/punch-in
  "ocs" 'bh/punch-out
  "oco" 'org-clock-out
  "ocg" 'org-clock-goto
  "occ" 'org-clock-cancel)


(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-j") 'newline-and-indent)

(evil-define-key 'normal global-map "K" 'helm-show-kill-ring)

(spacemacs/set-leader-keys "om" 'op/copy-last-message)

(define-key evil-normal-state-map (kbd "M-DEL") 'evil-window-map)


(evil-define-key 'insert global-map (kbd "C-e") 'move-end-of-line)
(evil-define-key 'insert global-map (kbd "C-a") 'beginning-of-line)
(evil-define-key 'insert global-map (kbd "C-k") 'kill-line)
