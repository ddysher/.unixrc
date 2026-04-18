;;------------------------------------------------------------------------------
;; Provide magit mode - a mode to work with git
;;------------------------------------------------------------------------------
(use-package magit
  :bind (("C-c g" . magit-status)
         ("C-c C-g" . magit-status))
  :config
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer
           buffer (if (and (derived-mode-p 'magit-mode)
                           (memq (with-current-buffer buffer major-mode)
                                 '(magit-process-mode
                                   magit-revision-mode
                                   magit-diff-mode
                                   magit-stash-mode
                                   magit-status-mode)))
                      nil
                    '(display-buffer-same-window))))))

(provide 'init-magit)
