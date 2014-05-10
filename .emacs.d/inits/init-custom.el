;;------------------------------------------------------------------------------
;; Provide some general customization
;;------------------------------------------------------------------------------
;; Enable/ Disable some minor mode
(menu-bar-mode -99)                ; disable menu (useful in terminal)
(column-number-mode)               ; show column number
(global-auto-revert-mode t)        ; enable auto revert (e.g. git)
(if window-system
    (progn (tool-bar-mode -1)           ; disable tool bar
	   (scroll-bar-mode -1)))           ; disable scroll bar


;; Global settings
;; set tab width globally, some other mode can change this locally.
;; note the  "default-tab-with" variable is obsolete
(setq-default tab-width universal-indent-size)
;; use spaces where tab is needed
(setq-default indent-tabs-mode nil)
;; no backup files (which end with ~)
(setq make-backup-files nil)
;; no autosave files (surrounded by #)
(setq auto-save-default nil)
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
;; turn off wrapping for long lines
(set-default 'truncate-lines t)
;; answer `y` & `n` for `yes` & `no
(defalias 'yes-or-no-p 'y-or-n-p)
;; start server, used by emacsclient. `emacs` is alias to `emacsclient`
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))


;; Associate config mode to ".zsh-theme, .defs, BUILD, etc."
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.defs$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitmodules$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\BUILD$" . conf-mode))

;; Show trailing whitespaces on specific modes, hooks are added collectively
;; at this init-custom file, instead of adding at every mode. Note using
;; (setq-default show-trailing-whitespace t) will enable it in all buffers
(add-hook 'c-mode-common-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'lua-mode-hook  (lambda () (setq show-trailing-whitespace t)))
(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'js-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'latex-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'asm-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'markdown-mode-hook (lambda () (setq show-trailing-whitespace t)))
;;(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))

;; Highlight current line in every mode instead of term-mode and minibuffer;
;; using (global-hl-line-mode 1) will enable it in all buffers, and is not
;; overridable by hl-line-mode variable
(add-hook 'minibuffer-setup-hook '(lambda () (hl-line-mode 0)))
(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (hl-line-mode
              (if (or (equal major-mode 'term-mode)
                      (equal major-mode 'erc-mode))
                  0
                1))))

;; Disable bell when hit c-g
(setq ring-bell-function 'ignore)

;; Kill all buffers except current active one
(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Make active buffer more highlighted!
(set-face-attribute 'mode-line
                    nil
                    :foreground "gray80"
                    :background "IndianRed4"
                    :box '(:line-width 1 :style released-button))
(set-face-attribute 'mode-line-inactive
                    nil
                    :foreground "#5F7F5F" ; color borrowed from zenburn-theme.el
                    :background "#383838"
                    :box '(:line-width 1 :style released-button))


(provide 'init-custom)
