;;------------------------------------------------------------------------------
;; Provide tramp, tramp mode is built-in with emacs. It works even not required
;; explicitly.  Basic usage:
;;   C-x C-f /remotehost:filename  RET (or /method:user@remotehost:filename)
;; E.g:
;;   C-x C-f [F1] -> /deyuan.me:~/Documents/file  RET
;;   C-x C-f [F1] -> /ssh:root@deyuan.me:~/Documents/file  RET
;;------------------------------------------------------------------------------
(require 'tramp)


(setq tramp-default-method "ssh")


(provide 'init-tramp)
