;;------------------------------------------------------------------------------
;; Provide auto-complete mode
;;------------------------------------------------------------------------------
(require-package 'auto-complete)
(require 'auto-complete-config)


(ac-config-default)
(global-auto-complete-mode t)
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)


(provide 'init-auto-complete)
