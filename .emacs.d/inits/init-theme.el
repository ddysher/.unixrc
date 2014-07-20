;;------------------------------------------------------------------------------
;; Load theme and font
;;------------------------------------------------------------------------------
(require-package 'zenburn-theme)


(load-theme 'zenburn t)
;; Make active buffer more highlighted, used with zenburn.
(set-face-attribute 'mode-line
                    nil
                    :foreground "gray80"
                    :background "IndianRed4"
                    :box '(:line-width 1 :style released-button))
(set-face-attribute 'mode-line-inactive
                    nil
                    :foreground "#5F7F5F" ; color borrowed from zenburn-theme.el
                    :background "#383838"
                    :box '(:line-width 1 :style released-button))

(cond (*linux*  (set-default-font "Inconsolata-10")))
(cond (*macpro* (set-default-font "Monaco-11")))
(cond (*macair* (set-default-font "Monaco-12")))


(provide 'init-theme)
