;;------------------------------------------------------------------------------
;; General customization: UI, editor behavior, and environment.
;;------------------------------------------------------------------------------

;;; UI and display.
;; menu-bar, tool-bar, scroll-bar are disabled in early-init.el for faster startup.
(menu-bar-mode -99)               ; also disable in terminal frames
(column-number-mode)              ; show column number in modeline
(global-auto-revert-mode t)       ; auto-reload buffers changed on disk (e.g. git)
(setq auto-revert-check-vc-info t)
(setq frame-resize-pixelwise t)   ; avoid blank line in fullscreen mode
(setq-default display-fill-column-indicator-column 80)
(winner-mode 1)                   ; window layout undo/redo

;;; Editor behavior.
;; setq-default sets buffer-local defaults; setq for global variables.
(setq-default tab-width universal-indent-size)
(setq-default indent-tabs-mode nil)        ; use spaces instead of tabs
(setq make-backup-files nil)               ; no backup~ files; use version control
(setq auto-save-default nil)               ; no #autosave# files
(setq inhibit-splash-screen t)             ; go straight to *scratch*
(setq show-help-function nil)              ; disable modeline tooltips
(setq ns-pop-up-frames nil)                ; reuse existing frame when opening files
(setq confirm-nonexistent-file-or-buffer nil)
(setq ring-bell-function 'ignore)          ; no bell on C-g
(defalias 'yes-or-no-p 'y-or-n-p)

;;; System and environment.
(setenv "LANG" "en_US.UTF-8")
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;;; Utilities.
;; Display ANSI color codes in Emacs buffers (useful for log files).
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'init-custom)
