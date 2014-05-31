;;------------------------------------------------------------------------------
;; Provide objective-c mode, objective-c mode is built-in emacs mode
;;------------------------------------------------------------------------------
(defun my-objc-mode-hook ()
  (setq c-basic-offset universal-indent-size))

(add-hook 'objc-mode-hook 'my-objc-mode-hook)


(provide 'init-objc-mode)
