;;------------------------------------------------------------------------------
;; Provide smartparens mode, a minor mode for dealing with parens pairs and
;; tries to be smart about it. It's elpa managed mode.
;;------------------------------------------------------------------------------
(require-package 'smartparens)
(require 'smartparens)


(smartparens-global-mode)
(require 'smartparens-config)


(provide 'init-smartparens-mode)
