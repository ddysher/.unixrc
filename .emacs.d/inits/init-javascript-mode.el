;;------------------------------------------------------------------------------
;; Provide javascript mode, javascript-mode is built-in emacs mode
;;------------------------------------------------------------------------------
(defun my-js-mode-hook ()
  ;;(local-set-key "\C-m" 'newline-and-indent)
  (setq js-indent-level universal-indent-size))


(add-hook 'js-mode-hook 'my-js-mode-hook)


(provide 'init-javascript-mode)
