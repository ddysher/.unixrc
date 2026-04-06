;;------------------------------------------------------------------------------
;; Provide smex mode. Smex is a M-x enhancement for Emacs. Built on top of Ido,
;; it provides a convenient interface to your recently and most frequently used
;; commands. And to all the other commands, too. It's elpa managed mode.
;;------------------------------------------------------------------------------
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(provide 'init-smex)
