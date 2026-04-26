;;------------------------------------------------------------------------------
;; Provide magit mode - a mode to work with git
;;------------------------------------------------------------------------------

(use-package magit
  :bind (("C-c g" . magit-status)
         ("C-c C-g" . magit-status))
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

(provide 'init-magit)
