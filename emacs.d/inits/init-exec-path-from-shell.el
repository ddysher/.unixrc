;;------------------------------------------------------------------------------
;; Provide exec path from shell, see:
;; https://github.com/purcell/exec-path-from-shell
;;------------------------------------------------------------------------------
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables
        (append exec-path-from-shell-variables
                (if *darwin*
                    '("GOPATH" "GOROOT")
                  '("GOPATH"))))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path-from-shell)
