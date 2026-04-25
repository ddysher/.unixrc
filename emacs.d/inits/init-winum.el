;;------------------------------------------------------------------------------
;; Provide winum mode, use M-1 ~ M-0 to switch windows
;; Migrated from window-numbering (unmaintained since 2016) to winum.
;;------------------------------------------------------------------------------

(use-package winum
  :config
  ;; Reserve 0 for the minibuffer when it is active; other windows keep the
  ;; default sequential numbering.
  (setq winum-auto-assign-0-to-minibuffer t)

  ;; Enable the global minor mode so winum starts tracking windows and its
  ;; keymap becomes active.
  (winum-mode 1)

  ;; Use the winum minor-mode map so Meta-digit overrides Emacs's default
  ;; numeric prefix argument bindings.
  (define-key winum-keymap (kbd "M-0") #'winum-select-window-0)
  (define-key winum-keymap (kbd "M-1") #'winum-select-window-1)
  (define-key winum-keymap (kbd "M-2") #'winum-select-window-2)
  (define-key winum-keymap (kbd "M-3") #'winum-select-window-3)
  (define-key winum-keymap (kbd "M-4") #'winum-select-window-4)
  (define-key winum-keymap (kbd "M-5") #'winum-select-window-5)
  (define-key winum-keymap (kbd "M-6") #'winum-select-window-6)
  (define-key winum-keymap (kbd "M-7") #'winum-select-window-7)
  (define-key winum-keymap (kbd "M-8") #'winum-select-window-8)
  (define-key winum-keymap (kbd "M-9") #'winum-select-window-9))

(provide 'init-winum)
