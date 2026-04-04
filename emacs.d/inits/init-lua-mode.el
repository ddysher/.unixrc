;;------------------------------------------------------------------------------
;; Provide lua mode, lua-mode is elpa managed package
;;------------------------------------------------------------------------------
(use-package lua-mode
  :defer t
  :hook (lua-mode . (lambda () (setq lua-indent-level universal-indent-size))))

(provide 'init-lua-mode)
