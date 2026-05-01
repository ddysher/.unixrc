;;------------------------------------------------------------------------------
;; Provide magit mode - a mode to work with git
;;------------------------------------------------------------------------------

(use-package magit
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

(provide 'init-magit)
