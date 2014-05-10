;;------------------------------------------------------------------------------
;; Provide python mode, python-mode is built-in emacs mode
;;------------------------------------------------------------------------------
(defun my-python-mode-hook ()
  (local-set-key "\C-m" 'newline-and-indent)
  (setq python-indent-offset universal-indent-size))

(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))
(add-hook 'python-mode-hook 'my-python-mode-hook)


(provide 'init-python-mode)
