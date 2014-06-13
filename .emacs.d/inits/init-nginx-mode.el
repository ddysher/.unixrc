;;------------------------------------------------------------------------------
;; Provide nginx mode, a small major mode for editing nginx config files. It's
;; elpa managed mode. The auto load feature should automatically activate for
;; files called nginx.conf and files under /etc/nginx, if not, then add the
;; following:
;;   (add-to-list 'auto-mode-alist
;;                '("/etc/nginx/sites-available/.*" . nginx-mode))
;;------------------------------------------------------------------------------
(require-package 'nginx-mode)
(require 'nginx-mode)


;; Apply nginx-mode to entire nginx directory (including sub-directories)
(add-to-list 'auto-mode-alist '("/etc/nginx/.*" . nginx-mode))


(provide 'init-nginx-mode)
