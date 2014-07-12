;;------------------------------------------------------------------------------
;; Provide shell script mode, sh-mode is built-in emacs mode.
;;------------------------------------------------------------------------------
(defun sh-mode-custom-hook ()
  (setq sh-indentation universal-indent-size)
  (setq sh-basic-offset universal-indent-size))

(add-hook 'sh-mode-hook 'sh-mode-custom-hook)
(add-hook 'sh-mode-hook 'sh-mode-custom-hook)


(provide 'init-sh-mode)
