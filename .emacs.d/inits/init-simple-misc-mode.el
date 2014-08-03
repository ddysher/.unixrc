;;------------------------------------------------------------------------------
;; Provide simple mode that do not need configurations.
;;------------------------------------------------------------------------------
(require-package 'markdown-mode)
(require 'markdown-mode)

(require-package 'coffee-mode)
(require 'coffee-mode)

(require-package 'jade-mode)
(require 'jade-mode)

(require-package 'go-mode)
(require 'go-mode)

(require-package 'php-mode)
(require 'php-mode)

(require-package 'thrift)
(require 'thrift)

;; Apply apache-mode to entire apache2 directory (including sub-directories)
(require-package 'apache-mode)
(require 'apache-mode)
(add-to-list 'auto-mode-alist '("/etc/apache2/.*" . apache-mode))

;; Apply nginx-mode to entire nginx directory (including sub-directories)
(require-package 'nginx-mode)
(require 'nginx-mode)
(add-to-list 'auto-mode-alist '("/etc/nginx/.*" . nginx-mode))

;; Temporary & pending mode
;; (require 'flex-mode)
;; (require 'cool-mode)
;; (require 'bison-mode)
;; (require 'init-multi-web)


(provide 'init-simple-misc-mode)
