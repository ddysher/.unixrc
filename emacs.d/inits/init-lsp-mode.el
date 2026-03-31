;;------------------------------------------------------------------------------
;; Provide lsp mode, managed by MELPA.
;;------------------------------------------------------------------------------
(require-package 'lsp-mode)

;; Defer lsp-mode until a supported mode triggers it via hook.
(add-hook 'go-mode-hook #'lsp)

(with-eval-after-load 'lsp-mode
  (setq lsp-enable-file-watchers nil))

(provide 'init-lsp-mode)
