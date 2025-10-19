;;------------------------------------------------------------------------------
;; Provide exec path from shell, see:
;; https://github.com/purcell/exec-path-from-shell
;;------------------------------------------------------------------------------
(require-package 'exec-path-from-shell)
(require 'exec-path-from-shell)

;; For both Mac and Linux, load shell path if starting from window.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; 'GOPATH' should always be set in .zshrc.
(exec-path-from-shell-copy-env "GOPATH")

;; On Mac, go is managed by homebrew, 'GOROOT' is required for godef.
(when *darwin*
  (exec-path-from-shell-copy-env "GOROOT"))

(provide 'init-exec-path-from-shell)
