;;------------------------------------------------------------------------------
;; Provide lua mode, lua-mode is elpa managed package
;;------------------------------------------------------------------------------
(require-package 'lua-mode)
(require 'lua-mode)


(defun my-lua-mode-hook ()
  ;; set lua's indent
  (setq lua-indent-level universal-indent-size))

(add-hook 'lua-mode-hook 'my-lua-mode-hook)


(provide 'init-lua-mode)
