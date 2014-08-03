;;------------------------------------------------------------------------------
;; Provide auto-complete mode
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

;; Force an auto-complete, C-o was bound to open-line.
(define-key ac-mode-map (kbd "C-o") 'auto-complete)

;; Faster shown menu map.
(setq ac-delay 0.1)
(setq ac-auto-show-menu 0.3)
(setq ac-quick-help-delay 0.5)

;; Distinguish case, default is smart.
(setq ac-ignore-case nil)


(provide 'init-auto-complete)
