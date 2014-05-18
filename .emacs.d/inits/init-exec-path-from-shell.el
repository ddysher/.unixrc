;;------------------------------------------------------------------------------
;; Provide exec path from shell, see:
;; https://github.com/purcell/exec-path-from-shell
;;------------------------------------------------------------------------------
(require-package 'exec-path-from-shell)
(require 'exec-path-from-shell)


(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(provide 'init-exec-path-from-shell)
