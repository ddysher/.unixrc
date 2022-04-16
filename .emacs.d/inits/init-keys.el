;;------------------------------------------------------------------------------
;; Keyboard bindings
;;------------------------------------------------------------------------------
;; Global key bindings
(global-set-key [f1] 'find-file)        ; M-x open file (C-x-f)
(global-set-key [f2] 'save-buffer)      ; M-x save file (C-x-s)
(global-set-key [f3] 'other-window)     ; M-x other-window (C-x o)
(global-set-key [f4] 'multi-vterm-dedicated-open)
(global-set-key [f5] 'multi-vterm)
(global-set-key [f6] 'replace-regexp)
(global-set-key [f7] 'ag-project)
(global-set-key [f8] 'register-to-point) ; Jump to win conf (C-x r j)
(global-set-key [f9] 'bookmark-jump) ; M-x bookmark-jump (C-x r b) (C-x r m)
(global-set-key [f10] 'delete-trailing-whitespace)
(global-set-key [f12] 'revert-buffer)
(global-set-key (kbd "ESC ESC") 'abort-recursive-edit) ; same as "C-g"
;; set M-p the same as "C-p + C-l" to relief finger (no original M-p binding)
(global-set-key (kbd "M-p") 'scroll-down-in-place)
;; set M-n the same as "C-n + C-l" to relief finger (no original M-n binding)
(global-set-key (kbd "M-n") 'scroll-up-in-place)
;; set M-a the same as C-a to relief finger (original M-a move backward sentence)
(global-set-key (kbd "M-a") 'move-beginning-of-line)
;; set M-e the same as C-e to relief finger (original M-e move forward sentence)
(global-set-key (kbd "M-e") 'move-end-of-line)
;; pop up buffer selection in mini buffer.
(global-set-key (kbd "C-o") 'switch-to-buffer)
;; search current buffer for matching pattern.
(global-set-key (kbd "C-c s") 'search-buffer)
;; search all buffers for matching pattern.
(global-set-key (kbd "C-c C-s") 'search-all-buffers)
;; start a personal workspace.
(global-set-key (kbd "C-c C-e") 'start-workspace)
;; list all buffers in new a window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c C-b") 'buffer-menu)
;; navigate buffer quickly
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-/") 'next-buffer)
;; winner mode key bindings.
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

;; add key bindings to window numbering mode (apart from M-1, M-2, etc)
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

;; Key bindings for Mac
(if *darwin*
    (progn
      (setq mac-command-modifier 'meta) ; set the Command key as Meta
      (setq mac-control-modifier 'ctrl) ; set the Control key as Control
      (setq mac-option-modifier 'super) ; set the left Option key as Super
      (setq mac-right-option-modifier 'hyper) ; set the right Option key as Hyper
      ;; control window size ('s' is Super)
      (global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
      (global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
      (global-set-key (kbd "C-s-<down>") 'shrink-window)
      (global-set-key (kbd "C-s-<up>") 'enlarge-window)))

(provide 'init-keys)
