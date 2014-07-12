;;------------------------------------------------------------------------------
;; Load theme and font
;;------------------------------------------------------------------------------
(require-package 'zenburn-theme)


(load-theme 'zenburn t)
(cond (*linux*  (set-default-font "Inconsolata-10")))
(cond (*macpro* (set-default-font "Monaco-11")))
(cond (*macair* (set-default-font "Monaco-12")))


(provide 'init-theme)
