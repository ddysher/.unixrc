;;------------------------------------------------------------------------------
;; Provide exec path from shell, see:
;; https://github.com/purcell/exec-path-from-shell
;;------------------------------------------------------------------------------
(require-package 'exec-path-from-shell)
(require 'exec-path-from-shell)


(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(if *home-desktop*
    (exec-path-from-shell-copy-env "GOPATH"))


(provide 'init-exec-path-from-shell)
