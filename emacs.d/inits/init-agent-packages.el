;;------------------------------------------------------------------------------
;; claudemacs, managing coding agent sessions in emacs
;;------------------------------------------------------------------------------
(use-package claudemacs
  :defer t
  :vc (:url "https://github.com/cpoile/claudemacs"))

;;------------------------------------------------------------------------------
;; agent-shell, managing coding agent sessions in emacs using ACP
;;------------------------------------------------------------------------------
(use-package agent-shell
  :defer t)

(provide 'init-agent-packages)
