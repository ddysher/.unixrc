;;------------------------------------------------------------------------------
;; Provide apache mode, a small major mode for editing apache config files. It's
;; elpa managed mode.
;;------------------------------------------------------------------------------
(require-package 'apache-mode)
(require 'apache-mode)


;; Apply apache-mode to entire apache2 directory (including sub-directories)
(add-to-list 'auto-mode-alist '("/etc/apache2/.*" . apache-mode))


(provide 'init-apache-mode)
