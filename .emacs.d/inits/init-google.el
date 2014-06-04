;;------------------------------------------------------------------------------
;; Define helper functions for google's working environment.
;;------------------------------------------------------------------------------
(defun g4-edit-current ()
    (shell-command (format "g4 edit %s" (buffer-file-name)))
    (revert-buffer))


(global-set-key [f12] 'g4-edit-current)


(provide 'init-google)

