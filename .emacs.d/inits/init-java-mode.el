;;------------------------------------------------------------------------------
;; Provide java mode, java-mode is built-in emacs mode.
;;------------------------------------------------------------------------------
(defun java-mode-custom-hook ()
  (setq c-basic-offset universal-indent-size)
  (setq tab-width universal-indent-size)
  (local-set-key "\C-m" 'newline-and-indent)) ; indent next line properly

(add-hook 'java-mode-hook 'java-mode-custom-hook)


(provide 'init-java-mode)
