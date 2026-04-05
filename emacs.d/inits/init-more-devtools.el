;;------------------------------------------------------------------------------
;; Dev-related modes that do not need extensive configuration.
;; All packages are deferred — they load when their file types are opened
;; or their commands are invoked.
;;------------------------------------------------------------------------------

;; Markup and data format modes.
(use-package wsd-mode :defer t)
(use-package thrift :defer t)
(use-package yaml-mode
  :hook (yaml-mode . (lambda () (run-hooks 'prog-mode-hook))))
(use-package dockerfile-mode :defer t)
(use-package protobuf-mode
  :hook (protobuf-mode . (lambda () (setq c-basic-offset universal-indent-size))))

;; Scripting and config modes.
(use-package apache-mode
  :mode ("/etc/apache2/.*" . apache-mode))
(use-package nginx-mode
  :mode ("/etc/nginx/.*" . nginx-mode))
(use-package php-mode :defer t)
(use-package matlab-mode
  :mode ("\\.m\\'" . matlab-mode))

;; Shell scripting (built-in sh-mode).
(defun sh-mode-custom-hook ()
  (setq sh-indentation universal-indent-size)
  (setq sh-basic-offset universal-indent-size))
(add-hook 'sh-mode-hook 'sh-mode-custom-hook)

;; Lua.
(use-package lua-mode
  :hook (lua-mode . (lambda () (setq lua-indent-level universal-indent-size))))

;; Scala.
(use-package scala-mode :defer t)
(use-package sbt-mode :defer t)

;; Dev tools.
(use-package flycheck :defer t)
(use-package ag
  :defer t
  :config
  (setq ag-reuse-window 't)
  (setq ag-reuse-buffers 't))

;; Utility modes.
(use-package hackernews :defer t)
(use-package neotree :defer t)

(provide 'init-more-devtools)
