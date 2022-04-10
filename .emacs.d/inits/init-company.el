;;------------------------------------------------------------------------------
;; Provide company mode, installed via MELPA.
;;------------------------------------------------------------------------------
(require-package 'company)

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company)
