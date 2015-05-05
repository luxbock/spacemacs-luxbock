;;; Leader

(spacemacs/declare-prefix "y" "yasnippet")

(evil-leader/set-key
  ;; Preserve my own window-layout
  "w." 'spacemacs/window-manipulation-micro-state
  "ww" 'helm-mini
  "w2" 'layout-double-columns
  "w3" 'layout-triple-columns
  "wc" 'evil-window-new
  "wD" 'kill-buffer-and-window
  "ws" 'cofi/smart-split
  "wS" 'split-window-below
  "wv" 'split-window-right-and-focus
  "w/" 'split-window-right-and-focus
  "w-" 'split-window-below-and-focus
  ;; "wa" 'ace-window
  "wH" 'evil-window-move-far-left
  "wh" 'evil-window-left
  "wJ" 'evil-window-move-very-bottom
  "wj" 'evil-window-down
  "wK" 'evil-window-move-very-top
  "wk" 'evil-window-up
  "wL" 'evil-window-move-far-right
  "wl" 'evil-window-right
  "wu" 'winner-undo
  "wU" 'winner-redo
  "wo" 'other-window
  "wf" 'other-frame
  "wR" 'rotate-windows

  "ff" 'helm-find-files
  "fS" 'save-some-buffers

  "nd" 'narrow-or-widen-dwim

  "yn" 'yas-new-snippet
  "yt" 'tiny-expand

  "bk" 'kill-buffer

  "ty" 'yas-minor-mode

  "om" 'op/copy-last-message
  "ow" 'whitespace-cleanup

  "oa"  'luxbock/switch-to-default-agenda-view
  "ot"  'org-capture
  "oci" 'bh/punch-in
  "ocs" 'bh/punch-out
  "oco" 'org-clock-out
  "ocg" 'org-clock-goto
  "occ" 'org-clock-cancel)

(global-set-key (kbd "C-x C-k") 'kill-buffer)

(evil-define-key 'normal global-map "K" 'helm-show-kill-ring)

;; (evil-leader/set-key "om" 'op/copy-last-message)

;; Company-mode
;; (define-key company-active-map (kbd "C-k") nil)
;; (define-key company-active-map (kbd "C-j") 'newline-and-indent)
;; (define-key company-active-map (kbd "M-j") 'company-select-next)
;; (define-key company-active-map (kbd "M-k") 'company-select-previous)
;; (define-key company-active-map (kbd "C-/") 'company-search-candidates)
;; (define-key company-active-map (kbd "C-M-/") 'company-filter-candidates)
;; (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
;; (define-key company-active-map (kbd "C-w") 'subword-backward-kill)
