;;------------------------------------------------------------------------------
;; Provide lsp mode, managed by MELPA.
;;------------------------------------------------------------------------------
;; lsp-mode is kept available as an alternative to eglot (built-in).
;; Currently no languages are hooked to lsp-mode; go and python use eglot.
;; Switch a language back to lsp-mode by adding its hook here, e.g.:
;;   :hook (go-mode . lsp)
;;------------------------------------------------------------------------------
(use-package lsp-mode
  :config
  (setq lsp-enable-file-watchers nil))

(provide 'init-lsp-mode)
