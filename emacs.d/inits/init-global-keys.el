;;------------------------------------------------------------------------------
;; Global keybindings: Function keys, Navigation, Buffer management, Search
;; Window management, MacOS only key remap, etc.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Function keys for commonly used functions
;;------------------------------------------------------------------------------
(global-set-key [f1]  'find-file)
(global-set-key [f2]  'save-buffer)
(global-set-key [f3]  'kill-current-buffer)
(global-set-key [f4]  'revert-buffer)
(global-set-key [f5]  'consult-line-multi)
(global-set-key [f6]  'consult-ripgrep)
(global-set-key [f7]  'query-replace-regexp)
(global-set-key [f8]  'delete-trailing-whitespace)
(global-set-key [f9]  'dirvish)
(global-set-key [f12] 'multi-vterm)

;;------------------------------------------------------------------------------
;; Navigation
;;------------------------------------------------------------------------------
;; ESC ESC as an alternative to C-g for aborting minibuffer/recursive edits.
(global-set-key (kbd "ESC ESC") 'abort-recursive-edit)

;; Scroll without moving point; no default bindings for M-p/M-n globally.
(global-set-key (kbd "M-p") 'scroll-down-in-place)
(global-set-key (kbd "M-n") 'scroll-up-in-place)

;; Previous/next buffer.
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)

;;------------------------------------------------------------------------------
;; Buffer management
;;------------------------------------------------------------------------------
;; Unified picker: open buffers, recent files, and bookmarks in one command.
(global-set-key (kbd "C-o")     'consult-buffer)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)          ; remap to better ibuffer

;;------------------------------------------------------------------------------
;; Search (consult) and Actions (embark)
;;------------------------------------------------------------------------------
;; Consult search
(global-set-key (kbd "M-s l") 'consult-line)       ; search in current buffer
(global-set-key (kbd "M-s m") 'consult-line-multi) ; search in multiple buffers
(global-set-key (kbd "M-s r") 'consult-ripgrep)    ; search in project/current dir with ripgrep
(global-set-key (kbd "M-s g") 'consult-git-grep)   ; search in project/current dir with git grep

;; Embark action
(global-set-key (kbd "C-;")   'embark-act)         ; context-sensitive actions on thing at point

;;------------------------------------------------------------------------------
;; Window management
;;------------------------------------------------------------------------------
;; Directional focus with capital HJKL to avoid conflicts with M-h (mark-paragraph).
(global-set-key (kbd "M-H") 'windmove-left)
(global-set-key (kbd "M-J") 'windmove-down)
(global-set-key (kbd "M-K") 'windmove-up)
(global-set-key (kbd "M-L") 'windmove-right)

;; Winner mode: undo/redo window layout changes.
(global-set-key (kbd "C-x 4 u") 'winner-undo)
(global-set-key (kbd "C-x 4 r") 'winner-redo)

;;------------------------------------------------------------------------------
;; macOS key modifiers and window resizing
;;------------------------------------------------------------------------------
(when *darwin*
  (setq mac-command-modifier      'meta)   ; Command → Meta
  (setq mac-control-modifier      'ctrl)
  (setq mac-option-modifier       'super)  ; left Option → Super
  (setq mac-right-option-modifier 'hyper)  ; right Option → Hyper
  ;; Resize the focused window with Super + arrow keys.
  (global-set-key (kbd "C-s-<left>")  'shrink-window-horizontally)
  (global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
  (global-set-key (kbd "C-s-<down>")  'shrink-window)
  (global-set-key (kbd "C-s-<up>")    'enlarge-window))

(provide 'init-global-keys)
