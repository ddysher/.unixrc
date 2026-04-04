;;------------------------------------------------------------------------------
;; Provide lsp mode, managed by MELPA.
;;------------------------------------------------------------------------------
(use-package lsp-mode
  :hook (go-mode . lsp)
  :config
  (setq lsp-enable-file-watchers nil))

(provide 'init-lsp-mode)
