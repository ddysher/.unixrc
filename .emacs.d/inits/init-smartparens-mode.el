;;------------------------------------------------------------------------------
;; Provide smartparens mode, a minor mode for dealing with parens pairs and
;; tries to be smart about it. It's elpa managed mode.
;;------------------------------------------------------------------------------
(require-package 'smartparens)
(require 'smartparens)


;; Disable smartparens for now.
(when nil
  (require 'smartparens-config)
  (smartparens-global-mode t))


(provide 'init-smartparens-mode)
