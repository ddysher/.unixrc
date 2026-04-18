;;------------------------------------------------------------------------------
;; Dev-related modes and tools that do not need extensive configuration.
;; Packages are deferred — they load when their file types are opened
;; or their commands are invoked.
;;------------------------------------------------------------------------------

;;
;;; Prog mode hook: settings shared across all programming modes.
(defun prog-mode-custom-hook ()
  (setq show-trailing-whitespace t)
  (setq truncate-lines t)
  (tempel-abbrev-mode))

(add-hook 'prog-mode-hook 'prog-mode-custom-hook)

;;
;;; Language modes.

;; Data and markup formats.
(use-package yaml-mode
  :hook (yaml-mode . (lambda () (run-hooks 'prog-mode-hook))))
(use-package thrift :defer t)
(use-package protobuf-mode
  :hook (protobuf-mode . (lambda () (setq c-basic-offset universal-indent-size))))
(use-package dockerfile-mode :defer t)
(use-package wsd-mode :defer t)

;; Scripting languages.
(defun sh-mode-custom-hook ()
  (setq sh-indentation universal-indent-size)
  (setq sh-basic-offset universal-indent-size))
(add-hook 'sh-mode-hook 'sh-mode-custom-hook)

(use-package lua-mode
  :hook (lua-mode . (lambda () (setq lua-indent-level universal-indent-size))))

(use-package php-mode :defer t)

(use-package matlab-mode
  :mode ("\\.m\\'" . matlab-mode))

;; System language.
(defun java-mode-custom-hook ()
  (setq c-basic-offset universal-indent-size)
  (setq tab-width universal-indent-size)
  (local-set-key "\C-m" 'newline-and-indent)) ; indent next line properly

(add-hook 'java-mode-hook 'java-mode-custom-hook)

(use-package scala-mode :defer t)
(use-package sbt-mode :defer t)

(use-package rust-mode :defer t)

;; Infrastructure config.
(use-package apache-mode
  :mode ("/etc/apache2/.*" . apache-mode))
(use-package nginx-mode
  :mode ("/etc/nginx/.*" . nginx-mode))

;; File type associations.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitmodules$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\BUILD$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.defs$" . conf-mode))

;;
;;; Dev tools and utilities
(use-package flycheck :defer t)
(use-package ag
  :defer t
  :config
  (setq ag-reuse-window 't)
  (setq ag-reuse-buffers 't))

;; Utilities.
(use-package hackernews :defer t)
(use-package neotree :defer t)


(provide 'init-more-devconfigs)
