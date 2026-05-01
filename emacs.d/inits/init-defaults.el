;;------------------------------------------------------------------------------
;; Customization defaults: UI, editor behavior, environment, and server.
;;
;; Note: menu-bar, tool-bar, and scroll-bar are disabled in early-init.el
;; via default-frame-alist, covering both GUI and terminal frames.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; UI and display
;;------------------------------------------------------------------------------
(setq-default display-fill-column-indicator-column 80)
(setq auto-revert-check-vc-info t) ; refresh VC modeline state (branch, status) on revert
(column-number-mode 1)             ; shows current column
(global-auto-revert-mode 1) ; reload buffers changed on disk (e.g. after git checkout)
(winner-mode 1)             ; enables C-x 4 u/r for window layout undo/redo
(which-key-mode 1)          ; displays the key bindings

;;------------------------------------------------------------------------------
;; Editor behavior
;;------------------------------------------------------------------------------
(setq-default tab-width default-indent-size)
(setq-default indent-tabs-mode nil)     ; spaces only
(setq make-backup-files nil)            ; no backup~ files; use version control
(setq auto-save-default nil)            ; no #autosave# files
(setq create-lockfiles nil)             ; no #.lock files
(setq show-help-function nil)           ; suppress echo-area help for menu/toolbar items
(setq ns-pop-up-frames nil)             ; reuse existing frame when opening files from Finder
(setq confirm-nonexistent-file-or-buffer nil)
(setq ring-bell-function 'ignore)       ; silence C-g bell
(defalias 'yes-or-no-p 'y-or-n-p)       ; y/n suffices everywhere
(put 'erase-buffer 'disabled nil)       ; re-enable erase-buffer command

;; Track recently opened files.
(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 200)
  (recentf-exclude '("/tmp/" "COMMIT_EDITMSG\\'"))
  :init
  (recentf-mode 1))

;; Persist minibuffer history across sessions.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;;------------------------------------------------------------------------------
;; Emacs server for emacsclient
;;------------------------------------------------------------------------------
(require 'server)

(when (and (fboundp 'server-running-p)
           (not (server-running-p)))
  (server-start))

(provide 'init-defaults)
