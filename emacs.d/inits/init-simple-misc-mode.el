;;------------------------------------------------------------------------------
;; Provide simple mode that do not need configurations.
;;------------------------------------------------------------------------------
(require-package 'wsd-mode)
(require 'wsd-mode)

(require-package 'ein)
(require 'ein)

(require-package 'ag)
(require 'ag)

(require-package 'coffee-mode)
(require 'coffee-mode)

(require-package 'jade-mode)
(require 'jade-mode)

(require-package 'php-mode)
(require 'php-mode)

(require-package 'yaml-mode)
(require 'yaml-mode)
(add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

(require-package 'thrift)
(require 'thrift)

(require-package 'dockerfile-mode)
(require 'dockerfile-mode)

(require-package 'hackernews)
(require 'hackernews)

(require-package 'neotree)
(require 'neotree)

;; Apply apache-mode to entire apache2 directory (including sub-directories).
(require-package 'apache-mode)
(require 'apache-mode)
(add-to-list 'auto-mode-alist '("/etc/apache2/.*" . apache-mode))

;; Apply nginx-mode to entire nginx directory (including sub-directories).
(require-package 'nginx-mode)
(require 'nginx-mode)
(add-to-list 'auto-mode-alist '("/etc/nginx/.*" . nginx-mode))

(require-package 'matlab-mode)
(require 'matlab)
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

;; Disable flycheck by default (annoying for some buffer, enable as needed).
(require-package 'flycheck)

;; Start emacs server, to accept opening files from client, e.g. command line.
(require-package `edit-server)
(edit-server-start)

;; Winner Mode is a global minor mode. When activated, it allows you to "undo
;; (and "redo") changes in the window configuration with the key commands
;; 'C-c left'; and 'C-c right'.
(winner-mode 1)

;; Display ansi color in emacs buffer, instead of characters like '[3m]'.
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'init-simple-misc-mode)
