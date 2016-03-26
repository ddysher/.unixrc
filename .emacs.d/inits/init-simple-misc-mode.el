;;------------------------------------------------------------------------------
;; Provide simple mode that do not need configurations.
;;------------------------------------------------------------------------------
(require-package 'coffee-mode)
(require 'coffee-mode)

(require-package 'jade-mode)
(require 'jade-mode)

(require-package 'php-mode)
(require 'php-mode)

(require-package 'markdown-mode)
(require 'markdown-mode)
(add-hook 'markdown-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

(require-package 'yaml-mode)
(require 'yaml-mode)
(add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

(require-package 'thrift)
(require 'thrift)

(require-package 'dockerfile-mode)
(require 'dockerfile-mode)

(require-package 'magit)
(require 'magit)

(require-package 'hackernews)
(require 'hackernews)

;; Apply apache-mode to entire apache2 directory (including sub-directories).
(require-package 'apache-mode)
(require 'apache-mode)
(add-to-list 'auto-mode-alist '("/etc/apache2/.*" . apache-mode))

;; Apply nginx-mode to entire nginx directory (including sub-directories).
(require-package 'nginx-mode)
(require 'nginx-mode)
(add-to-list 'auto-mode-alist '("/etc/nginx/.*" . nginx-mode))

;; Disable flycheck by default (annoying for some buffer, enable as needed).
(require-package 'flycheck)

(require-package `edit-server)
(edit-server-start)

(winner-mode 1)

;; Temporary & pending mode
(when nil
  (require 'flex-mode)
  (require 'cool-mode)
  (require 'bison-mode))

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'init-simple-misc-mode)
