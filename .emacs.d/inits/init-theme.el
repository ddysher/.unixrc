;;------------------------------------------------------------------------------
;; Load theme and font
;;------------------------------------------------------------------------------
(require-package 'zenburn-theme)


(load-theme 'zenburn t)
(cond (*linux* (set-default-font "Inconsolata-10")))
(cond (*macpro*
       (set-default-font
        "-apple-Monaco-medium-normal-normal-*-11-*-*-*-m-0-iso10646-1")))
(cond (*macair*
       (set-default-font
        "-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")))


(provide 'init-theme)
