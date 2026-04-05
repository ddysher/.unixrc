;;------------------------------------------------------------------------------
;; Provide fill column indicator mode, which draw a line for 80 chars.
;; Use 'M-x display-fill-column-indicator-mode' to enable/disable.
;;------------------------------------------------------------------------------
;; Emacs 30 has built-in display-fill-column-indicator-mode, no package needed.
(setq-default display-fill-column-indicator-column 80)

(provide 'init-fill-column-indicator)
