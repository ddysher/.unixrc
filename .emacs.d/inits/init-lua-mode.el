;;------------------------------------------------------------------------------
;; Provide lua mode, lua-mode is elpa managed package
;;------------------------------------------------------------------------------
(require-package 'lua-mode)
(require 'lua-mode)


(defun lua-mode-custom-hook ()
  (setq lua-indent-level universal-indent-size))

(add-hook 'lua-mode-hook 'lua-mode-custom-hook)


(provide 'init-lua-mode)
