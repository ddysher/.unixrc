;;------------------------------------------------------------------------------
;; Provide lsp mode, managed by MELPA.
;;------------------------------------------------------------------------------
(require-package 'lsp-mode)
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp)

(provide 'init-lsp-mode)
