;;------------------------------------------------------------------------------
;; Provide lsp mode, managed by MELPA.
;;------------------------------------------------------------------------------
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp)

(provide 'init-lsp-mode)
