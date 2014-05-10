;;------------------------------------------------------------------------------
;; Provide cc mode, cc-mode is built-in emacs mode, for c, c++, java, etc
;;------------------------------------------------------------------------------
(defun my-cc-mode-hook ()
  (setq c-basic-offset universal-indent-size)
  (setq c-default-style "bsd")
  ;; indent next line properly
  (local-set-key "\C-m" 'newline-and-indent))

(add-hook 'c-mode-hook 'my-cc-mode-hook)
(add-hook 'c++-mode-hook 'my-cc-mode-hook)


(provide 'init-cc-mode)
