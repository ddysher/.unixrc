;;------------------------------------------------------------------------------
;; Provide ag, ag is elpa managed package.  This mode depends on external
;; program; to use this mode, host system must have 'ag' command installed.
;; It's useful to drop in a '.agignore' file to globally ignore search path.
;;------------------------------------------------------------------------------
(use-package ag
  :defer t
  :config
  (setq ag-reuse-window 't)
  (setq ag-reuse-buffers 't))

(provide 'init-ag-mode)
