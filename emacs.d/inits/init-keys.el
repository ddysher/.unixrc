;;------------------------------------------------------------------------------
;; Global keybindings.
;;
;; Function keys:
;;   F1-F12     - quick access to frequent commands (file, save, window, vterm, etc.)
;;
;; Navigation:
;;   ESC ESC    - abort (alternative to C-g)
;;   M-p/M-n    - scroll without moving point
;;   M-a/M-e    - line begin/end (overrides sentence movement)
;;   C-,/C-./C-/- previous/next buffer
;;
;; Buffer management (consult):
;;   C-o        - switch buffer / recent files / bookmarks
;;   C-x C-r    - open recent file
;;   C-x/C-c C-b - list all buffers
;;   C-c s      - search current buffer
;;   C-c C-s    - search all buffers
;;
;; Search (consult):
;;   M-s g/G/l/r - grep / git-grep / line / ripgrep
;;
;; Window management:
;;   M-H/J/K/L  - directional window focus (windmove)
;;   C-x 4 u/r  - winner undo/redo window layout
;;   C-c C-w    - start personal workspace
;;
;; macOS only:
;;   Command=Meta, left Option=Super, right Option=Hyper
;;   C-s-arrows - resize windows
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Function keys
;;------------------------------------------------------------------------------
(global-set-key [f1]  'find-file)
(global-set-key [f2]  'save-buffer)
(global-set-key [f3]  'other-window)
(global-set-key [f4]  'multi-vterm-dedicated-open)  ; toggle dedicated vterm window
(global-set-key [f5]  'multi-vterm)                 ; open a new vterm
(global-set-key [f6]  'replace-regexp)
(global-set-key [f7]  'ag-project)                  ; search in project root
(global-set-key [f8]  'register-to-point)           ; jump to saved window config
(global-set-key [f9]  'bookmark-jump)
(global-set-key [f10] 'delete-trailing-whitespace)
(global-set-key [f12] 'revert-buffer)


;;------------------------------------------------------------------------------
;; Navigation
;;------------------------------------------------------------------------------
;; ESC ESC as an alternative to C-g for aborting minibuffer/recursive edits.
(global-set-key (kbd "ESC ESC") 'abort-recursive-edit)

;; Scroll without moving point; no default bindings for M-p/M-n globally.
(global-set-key (kbd "M-p") 'scroll-down-in-place)
(global-set-key (kbd "M-n") 'scroll-up-in-place)

;; Line begin/end — overrides default backward/forward-sentence.
(global-set-key (kbd "M-a") 'move-beginning-of-line)
(global-set-key (kbd "M-e") 'move-end-of-line)

;; Previous/next buffer.
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-/") 'next-buffer)


;;------------------------------------------------------------------------------
;; Buffer management (consult)
;;------------------------------------------------------------------------------
;; Unified picker: open buffers, recent files, and bookmarks in one command.
(global-set-key (kbd "C-o")     'consult-buffer)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
;; Both chords open the buffer list for convenience from different modes.
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c C-b") 'buffer-menu)
(global-set-key (kbd "C-c s")   'search-buffer)
(global-set-key (kbd "C-c C-s") 'search-all-buffers)


;;------------------------------------------------------------------------------
;; Search (consult)
;;------------------------------------------------------------------------------
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-s G") 'consult-git-grep)
(global-set-key (kbd "M-s l") 'consult-line)
(global-set-key (kbd "M-s r") 'consult-ripgrep)


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

(global-set-key (kbd "C-c C-w") 'start-workspace)


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


(provide 'init-keys)
