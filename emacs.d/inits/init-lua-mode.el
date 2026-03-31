;;------------------------------------------------------------------------------
;; Provide lua mode, lua-mode is elpa managed package
;;------------------------------------------------------------------------------
(require-package 'lua-mode)

;; Defer config until a .lua file is opened.
(defun lua-mode-custom-hook ()
  (setq lua-indent-level universal-indent-size))

(add-hook 'lua-mode-hook 'lua-mode-custom-hook)

(provide 'init-lua-mode)
