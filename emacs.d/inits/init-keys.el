;;------------------------------------------------------------------------------
;; Keyboard bindings
;;------------------------------------------------------------------------------

;;; Function keys.
(global-set-key [f1] 'find-file)        ; open file (C-x C-f)
(global-set-key [f2] 'save-buffer)      ; save file (C-x C-s)
(global-set-key [f3] 'other-window)     ; switch to other window (C-x o)
(global-set-key [f4] 'multi-vterm-dedicated-open)
(global-set-key [f5] 'multi-vterm)
(global-set-key [f6] 'replace-regexp)
(global-set-key [f7] 'ag-project)
(global-set-key [f8] 'register-to-point) ; jump to window config register (C-x r j)
(global-set-key [f9] 'bookmark-jump)     ; jump to bookmark (C-x r b / C-x r m)
(global-set-key [f10] 'delete-trailing-whitespace)
(global-set-key [f12] 'revert-buffer)

;;; Navigation.
;; ESC ESC as an alternative to C-g for aborting.
(global-set-key (kbd "ESC ESC") 'abort-recursive-edit)
;; M-p/M-n: scroll without moving point, to relieve the finger (no default M-p/M-n binding).
(global-set-key (kbd "M-p") 'scroll-down-in-place)
(global-set-key (kbd "M-n") 'scroll-up-in-place)
;; M-a/M-e: move to line begin/end, to relieve the finger (default: move backward/forward sentence).
(global-set-key (kbd "M-a") 'move-beginning-of-line)
(global-set-key (kbd "M-e") 'move-end-of-line)
;; C-,/C-./C-/: navigate buffers quickly.
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-/") 'next-buffer)

;;; Buffer management.
;; Switch buffer in the minibuffer.
(global-set-key (kbd "C-o") 'switch-to-buffer)
;; List all buffers in a new window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c C-b") 'buffer-menu)
;; Search current buffer for matching pattern.
(global-set-key (kbd "C-c s") 'search-buffer)
;; Search all buffers for matching pattern.
(global-set-key (kbd "C-c C-s") 'search-all-buffers)

;;; Window management.
;; Winner mode: undo/redo window layout changes.
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)
;; Window numbering: jump to window by number (supplements default M-1, M-2, etc.).
(global-set-key (kbd "C-0") 'select-window-0)
(global-set-key (kbd "C-1") 'select-window-1)
(global-set-key (kbd "C-2") 'select-window-2)
(global-set-key (kbd "C-3") 'select-window-3)
(global-set-key (kbd "C-4") 'select-window-4)
(global-set-key (kbd "C-5") 'select-window-5)
(global-set-key (kbd "C-6") 'select-window-6)
(global-set-key (kbd "C-7") 'select-window-7)
(global-set-key (kbd "C-8") 'select-window-8)
(global-set-key (kbd "C-9") 'select-window-9)
;; Start a personal workspace.
(global-set-key (kbd "C-c C-w") 'start-workspace)

;;; macOS key modifiers and window resizing.
(when *darwin*
  (setq mac-command-modifier 'meta)       ; Command key as Meta
  (setq mac-control-modifier 'ctrl)       ; Control key as Control
  (setq mac-option-modifier 'super)       ; left Option key as Super
  (setq mac-right-option-modifier 'hyper) ; right Option key as Hyper
  ;; Resize windows with Super + arrow keys.
  (global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-s-<down>") 'shrink-window)
  (global-set-key (kbd "C-s-<up>") 'enlarge-window))

(provide 'init-keys)
