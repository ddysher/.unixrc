;;------------------------------------------------------------------------------
;; Provide fill column indicator mode, which draw a line for 80 chars.
;; Use 'M-x fci-mode' to enable/disable this minor mode.
;;------------------------------------------------------------------------------
(require-package 'fill-column-indicator)

(when (not *cygwin*)
  (require 'fill-column-indicator)
  (setq fci-rule-column 80))


(provide 'init-fill-column-indicator)
