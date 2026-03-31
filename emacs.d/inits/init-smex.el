;;------------------------------------------------------------------------------
;; Provide smex mode. Smex is a M-x enhancement for Emacs. Built on top of Ido,
;; it provides a convenient interface to your recently and most frequently used
;; commands. And to all the other commands, too. It's elpa managed mode.
;;------------------------------------------------------------------------------
(require-package 'smex)

;; Defer smex initialization until first M-x (auto-initializes on first use).
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'init-smex)
