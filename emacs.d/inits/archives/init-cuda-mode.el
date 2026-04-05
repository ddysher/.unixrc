;;------------------------------------------------------------------------------
;; Provide cuda mode, managed by melpa.
;;------------------------------------------------------------------------------
(require-package 'cuda-mode)
(require 'cuda-mode)

(add-to-list 'auto-mode-alist '("\\.cu\\'" . cuda-mode))

(provide 'init-cuda-mode)
