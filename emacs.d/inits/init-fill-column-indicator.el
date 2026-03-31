;;------------------------------------------------------------------------------
;; Provide fill column indicator mode, which draw a line for 80 chars.
;; Use 'M-x fci-mode' to enable/disable this minor mode.
;;------------------------------------------------------------------------------
;; Emacs 30 has built-in display-fill-column-indicator-mode, no package needed.
(setq-default display-fill-column-indicator-column 80)

(provide 'init-fill-column-indicator)
