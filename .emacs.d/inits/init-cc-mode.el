;;------------------------------------------------------------------------------
;; Provide cc mode, cc-mode is built-in emacs mode, for c, c++, java, etc
;;------------------------------------------------------------------------------
(defun cc-mode-custom-hook ()
  (setq c-basic-offset universal-indent-size)
  (setq c-default-style "bsd")
  (local-set-key "\C-m" 'newline-and-indent)) ; indent next line properly

(add-hook 'c-mode-hook 'cc-mode-custom-hook)
(add-hook 'c++-mode-hook 'cc-mode-custom-hook)


(provide 'init-cc-mode)
