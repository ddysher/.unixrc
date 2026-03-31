;;------------------------------------------------------------------------------
;; Provide simple mode that do not need configurations.
;; All packages are deferred — they load when their file types are opened
;; or their commands are invoked.
;;------------------------------------------------------------------------------
(require-package 'wsd-mode)
(require-package 'ein)
(require-package 'ag)
(require-package 'coffee-mode)
(require-package 'jade-mode)
(require-package 'php-mode)
(require-package 'yaml-mode)
(require-package 'thrift)
(require-package 'dockerfile-mode)
(require-package 'hackernews)
(require-package 'neotree)
(require-package 'apache-mode)
(require-package 'nginx-mode)
(require-package 'matlab-mode)
;; Disable flycheck by default (annoying for some buffer, enable as needed).
(require-package 'flycheck)

;; Start emacs server, to accept opening files from client, e.g. command line.
(require-package 'edit-server)

;; yaml-mode: add prog-mode-hook behaviors.
(add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

;; Apply apache-mode to entire apache2 directory (including sub-directories).
(add-to-list 'auto-mode-alist '("/etc/apache2/.*" . apache-mode))

;; Apply nginx-mode to entire nginx directory (including sub-directories).
(add-to-list 'auto-mode-alist '("/etc/nginx/.*" . nginx-mode))

;; matlab-mode: associate .m files.
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))

;; Start edit-server (for browser editing) after init to avoid blocking startup.
(add-hook 'after-init-hook
          (lambda ()
            (require 'edit-server)
            (edit-server-start)))

;; Winner Mode for window undo/redo.
(winner-mode 1)

;; Display ansi color in emacs buffer.
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'init-simple-misc-mode)
