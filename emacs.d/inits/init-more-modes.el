;;------------------------------------------------------------------------------
;; Provide simple mode that do not need configurations.
;; All packages are deferred — they load when their file types are opened
;; or their commands are invoked.
;;------------------------------------------------------------------------------
(use-package wsd-mode :defer t)
(use-package ein :defer t)
(use-package ag :defer t)
(use-package coffee-mode :defer t)
(use-package jade-mode :defer t)
(use-package php-mode :defer t)
(use-package yaml-mode
  :defer t
  :hook (yaml-mode . (lambda () (run-hooks 'prog-mode-hook))))
(use-package thrift :defer t)
(use-package dockerfile-mode :defer t)
(use-package hackernews :defer t)
(use-package neotree :defer t)
(use-package apache-mode
  :mode ("/etc/apache2/.*" . apache-mode))
(use-package nginx-mode
  :mode ("/etc/nginx/.*" . nginx-mode))
(use-package matlab-mode
  :mode ("\\.m\\'" . matlab-mode))
;; Disable flycheck by default (annoying for some buffer, enable as needed).
(use-package flycheck :defer t)

;; Start emacs server, to accept opening files from client, e.g. command line.
(use-package edit-server
  :hook (after-init . (lambda ()
                        (require 'edit-server)
                        (edit-server-start))))

;; Winner Mode for window undo/redo.
(winner-mode 1)

;; Display ansi color in emacs buffer.
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'init-more-modes)
