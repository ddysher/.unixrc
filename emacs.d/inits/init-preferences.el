;;------------------------------------------------------------------------------
;; General customization: UI, editor behavior, and environment.
;;
;; UI / display:
;;   column-number-mode, global-auto-revert-mode, winner-mode
;;   fill-column indicator at 80
;;
;; Editor behavior:
;;   spaces over tabs, no backup/autosave files, no bell, y/n instead of yes/no
;;
;; System / environment:
;;   LANG=en_US.UTF-8
;;
;; Utilities:
;;   display-ansi-colors — render ANSI escape codes in any buffer
;;
;; Note: menu-bar, tool-bar, and scroll-bar are disabled in early-init.el
;; via default-frame-alist, covering both GUI and terminal frames.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; UI and display
;;------------------------------------------------------------------------------
(setq auto-revert-check-vc-info t) ; refresh VC modeline state (branch, status) on revert
(setq-default display-fill-column-indicator-column 80)
(column-number-mode)             ; shows current column
(global-auto-revert-mode t)      ; reload buffers changed on disk (e.g. after git checkout)
(winner-mode 1)                  ; enables C-x 4 u/r for window layout undo/redo

;;------------------------------------------------------------------------------
;; Editor behavior (setq-default sets buffer-local defaults; setq sets global variables)
;;------------------------------------------------------------------------------
(setq-default tab-width universal-indent-size)
(setq-default indent-tabs-mode nil)     ; spaces only
(setq make-backup-files nil)            ; no backup~ files; use version control
(setq auto-save-default nil)            ; no #autosave# files
(setq show-help-function nil)           ; suppress echo-area help for menu/toolbar items
(setq ns-pop-up-frames nil)             ; reuse existing frame when opening files from Finder
(setq confirm-nonexistent-file-or-buffer nil)
(setq ring-bell-function 'ignore)       ; silence C-g bell
(defalias 'yes-or-no-p 'y-or-n-p)       ; y/n suffices everywhere

;;------------------------------------------------------------------------------
;; System and environment
;;------------------------------------------------------------------------------
(setenv "LANG" "en_US.UTF-8")

(put 'erase-buffer 'disabled nil)

;;------------------------------------------------------------------------------
;; Utilities
;;------------------------------------------------------------------------------
;; Render ANSI color codes in any buffer (useful for log files and shell output).
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'init-preferences)
