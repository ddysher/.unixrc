;;------------------------------------------------------------------------------
;; Provide exec path from shell, see:
;; https://github.com/purcell/exec-path-from-shell
;;------------------------------------------------------------------------------
(require-package 'exec-path-from-shell)
(require 'exec-path-from-shell)

;; Copy all needed env vars in a single shell invocation to avoid
;; spawning multiple subprocesses (each one costs ~0.3-0.5s).
(when (memq window-system '(mac ns x))
  (setq exec-path-from-shell-variables
        (append exec-path-from-shell-variables
                (if *darwin*
                    '("GOPATH" "GOROOT")
                  '("GOPATH"))))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path-from-shell)
