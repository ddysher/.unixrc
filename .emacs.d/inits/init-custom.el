;;------------------------------------------------------------------------------
;; Provide some general customization
;;------------------------------------------------------------------------------
;; Enable/ Disable some minor mode
(menu-bar-mode -99)                ; disable menu (useful in terminal)
(column-number-mode)               ; show column number
(global-auto-revert-mode t)        ; enable auto revert (e.g. git)
(setq auto-revert-check-vc-info t) ; check version control
(if window-system
    (progn
      (tool-bar-mode -1)                ; disable tool bar
      (scroll-bar-mode -1)))            ; disable scroll bar

;;
;; Global settings
;;
;; setq-default will set values for buffers that do not have their own
;; local variables, so we should use it for buffer-local variable.  For
;; non buffer-local variable, we can just use setq.
;;
;; set tab width globally, some other mode can change this locally.
;; note the  "default-tab-with" variable is obsolete.
(setq-default tab-width universal-indent-size)
;; use spaces where tab is needed
(setq-default indent-tabs-mode nil)
;; no backup files (which end with ~); prefer version control tool instead
(setq make-backup-files nil)
;; enable autosave files (surrounded by #); they will be deleted after save
(setq auto-save-default t)
;; only highlight line in active buffer
(setq hl-line-sticky-flag nil)
;; disable splash screen, enter *scratch* directly
(setq inhibit-splash-screen t)
;; disable the tooltips in modeline (better to use M-x tooltip-mode RET)
(setq show-help-function nil)
;; open in new buffer instead of new frame when open files in Window (Finder)
(setq ns-pop-up-frames nil)
;; do not need to confirm nonexistent file
(setq confirm-nonexistent-file-or-buffer nil)
;; disable bell when hit c-g
(setq ring-bell-function 'ignore)
;; answer `y` & `n` for `yes` & `no
(defalias 'yes-or-no-p 'y-or-n-p)
;; start server, used by emacsclient. `emacs` is alias to `emacsclient`
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))
;; associate config mode to ".zsh-theme, .defs, BUILD, etc."
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.defs$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitmodules$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\BUILD$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;
;; Prog mode settings
;;
(defun prog-mode-custom-hook ()
  "Hook for settings related to general programming modes."
  ;; show trailing whitespaces
  (setq show-trailing-whitespace t)
  ;; turn off wrapping for long lines
  (setq truncate-lines t)
  ;; highline current line
  (hl-line-mode 1))

(add-hook 'prog-mode-hook 'prog-mode-custom-hook)


(provide 'init-custom)
