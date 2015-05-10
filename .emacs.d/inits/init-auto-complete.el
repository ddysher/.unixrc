;;------------------------------------------------------------------------------
;; Provide auto-complete mode, installed via MELPA.
;;------------------------------------------------------------------------------
(require-package 'auto-complete)
(require 'auto-complete-config)


;; Enable auto completion globally, and use default setting.
(global-auto-complete-mode t)
(ac-config-default)

;; Enable menu map.
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;; Force an auto-complete.
(define-key ac-mode-map (kbd "C-c o") 'auto-complete)

;; Show menu map faster.
(setq ac-delay 0.1)
(setq ac-auto-show-menu 0.3)
(setq ac-quick-help-delay 0.5)

;; Distinguish case, default is smart.
(setq ac-ignore-case nil)

;; Custom dictionary for major mode.
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")


(provide 'init-auto-complete)
