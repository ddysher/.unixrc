;;------------------------------------------------------------------------------
;; Development utilities.
;;------------------------------------------------------------------------------

(use-package flycheck :defer t)

(use-package ag
  :defer t
  :config
  (setq ag-reuse-window 't)    ; open results in same window
  (setq ag-reuse-buffers 't))  ; reuse the *ag* buffer across searches

(use-package neotree :defer t)

(provide 'init-dev-utilities)
