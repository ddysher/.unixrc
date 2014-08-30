;;------------------------------------------------------------------------------
;; Keyboard bindings
;;------------------------------------------------------------------------------
;; Global key bindings
(global-set-key [f1] 'find-file)        ; M-x open file (C-x-f)
(global-set-key [f2] 'save-buffer)      ; M-x save file (C-x-s)
(global-set-key [f3] 'multi-term-dedicated)
(global-set-key [f4] 'multi-term)
(global-set-key [f5] 'other-window)     ; M-x other-window (C-x o)
(global-set-key [f6] 'replace-regexp)
(global-set-key [f7] 'window-configuration-to-register) ; Save win conf (C-x r w)
(global-set-key [f8] 'register-to-point) ; Jump to win conf (C-x r j)
(global-set-key [f9] 'bookmark-jump) ; M-x bookmark-jump (C-x r b) (C-x r m)
(global-set-key [f10] 'delete-trailing-whitespace)
(global-set-key [f12] 'revert-buffer)
(global-set-key (kbd "ESC ESC") 'abort-recursive-edit) ; same as "C-g"
;; set M-p the same as "C-p + C-l" to release finger (no original M-p binding)
(global-set-key (kbd "M-p") 'scroll-down-in-place)
;; set M-n the same as "C-n + C-l" to release finger (no original M-n binding)
(global-set-key (kbd "M-n") 'scroll-up-in-place)
;; set M-a the same as C-a to release finger (original M-a move backward sentence)
(global-set-key (kbd "M-a") 'move-beginning-of-line)
;; set M-e the same as C-e to release finger (original M-e move forward sentence)
(global-set-key (kbd "M-e") 'move-end-of-line)
(global-set-key (kbd "C-o") 'switch-to-buffer)
;; search current buffer for matching pattern.
(global-set-key (kbd "C-c s") 'search-buffer)
;; search all buffers for matching pattern.
(global-set-key (kbd "C-c C-s") 'search-all-buffers)
;; search all buffers for matching pattern.
(global-set-key (kbd "C-c C-e") 'start-workspace)
;; list all buffers in new a window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c C-b") 'buffer-menu)
;; winner mode key bindings.
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

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

(if *linux*
    (progn
      (setq mac-command-modifier 'meta) ; set the Command key as Meta
      (setq mac-control-modifier 'ctrl) ; set the Control key as Control
      (setq mac-option-modifier 'super) ; set the left Option key as Super
      (setq mac-right-option-modifier 'hyper) ; set the right Option key as Hyper
      ;; control window size ('s' is Super, bound to 'win')
      (global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
      (global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
      (global-set-key (kbd "s-<down>") 'shrink-window)
      (global-set-key (kbd "s-<up>") 'enlarge-window)))


(provide 'init-keys)
