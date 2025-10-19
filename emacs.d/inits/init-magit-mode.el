;;------------------------------------------------------------------------------
;; Provide magit mode - a mode to work with git
;;------------------------------------------------------------------------------
(require-package 'magit)
(require 'magit-mode)

;; By default, magit opens status buffer in another window. Here we reset
;; "magit-display-buffer-function" function to open status buffer in current
;; window, see https://github.com/magit/magit/issues/2541
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
                  '(display-buffer-same-window)))))

;; Key bindings for magit. magit-status is most commonly used.
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c C-g") 'magit-status)

(provide 'init-magit-mode)
