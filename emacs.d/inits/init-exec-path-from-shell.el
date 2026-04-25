;;------------------------------------------------------------------------------
;; Provide exec path from shell, see:
;; https://github.com/purcell/exec-path-from-shell
;;------------------------------------------------------------------------------

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  ;; Use non-interactive shell — much faster than the default interactive (-i) shell.
  ;; Requires env vars to be set in ~/.zshenv (sourced for non-interactive shells).
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(provide 'init-exec-path-from-shell)
