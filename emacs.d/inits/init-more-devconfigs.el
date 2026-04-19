;;------------------------------------------------------------------------------
;; Lightweight dev modes and tools that need little or no configuration.
;; Packages are deferred — they load when their file types are opened
;; or their commands are invoked.
;;
;; Prog mode hook (shared across all programming modes):
;;   show-trailing-whitespace, truncate-lines, tempel-abbrev-mode
;;
;; Data / markup:   yaml, thrift, protobuf, dockerfile, wsd
;; Scripting:       sh, lua, php, matlab
;; System:          java, scala/sbt, rust
;; Infra config:    apache, nginx
;; File types:      auto-mode-alist associations
;; Dev tools:       flycheck, ag
;; Utilities:       hackernews, neotree
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Prog mode hook
;;------------------------------------------------------------------------------
;; Applied to all programming modes; yaml-mode runs it explicitly via :hook
;; since it does not derive from prog-mode.
(defun prog-mode-custom-hook ()
  (setq show-trailing-whitespace t)
  (setq truncate-lines t)
  (tempel-abbrev-mode))

(add-hook 'prog-mode-hook 'prog-mode-custom-hook)


;;------------------------------------------------------------------------------
;; Data and markup formats
;;------------------------------------------------------------------------------
(use-package yaml-mode
  :hook (yaml-mode . (lambda () (run-hooks 'prog-mode-hook))))  ; not a prog-mode child
(use-package thrift :defer t)
(use-package protobuf-mode
  :hook (protobuf-mode . (lambda () (setq c-basic-offset universal-indent-size))))
(use-package dockerfile-mode :defer t)
(use-package wsd-mode :defer t)


;;------------------------------------------------------------------------------
;; Scripting languages
;;------------------------------------------------------------------------------
(defun sh-mode-custom-hook ()
  (setq sh-indentation universal-indent-size)
  (setq sh-basic-offset universal-indent-size))
(add-hook 'sh-mode-hook 'sh-mode-custom-hook)

(use-package lua-mode
  :hook (lua-mode . (lambda () (setq lua-indent-level universal-indent-size))))

(use-package php-mode :defer t)

(use-package matlab-mode
  :mode ("\\.m\\'" . matlab-mode))


;;------------------------------------------------------------------------------
;; System languages
;;------------------------------------------------------------------------------
(defun java-mode-custom-hook ()
  (setq c-basic-offset universal-indent-size)
  (setq tab-width universal-indent-size)
  (local-set-key "\C-m" 'newline-and-indent)) ; RET indents the new line

(add-hook 'java-mode-hook 'java-mode-custom-hook)

(use-package scala-mode :defer t)
(use-package sbt-mode :defer t)

(use-package rust-mode :defer t)


;;------------------------------------------------------------------------------
;; Infrastructure config
;;------------------------------------------------------------------------------
(use-package apache-mode
  :mode ("/etc/apache2/.*" . apache-mode))
(use-package nginx-mode
  :mode ("/etc/nginx/.*" . nginx-mode))


;;------------------------------------------------------------------------------
;; File type associations
;;------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitmodules$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\BUILD$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.defs$" . conf-mode))


;;------------------------------------------------------------------------------
;; Dev tools
;;------------------------------------------------------------------------------
(use-package flycheck :defer t)

(use-package ag
  :defer t
  :config
  (setq ag-reuse-window 't)    ; open results in same window
  (setq ag-reuse-buffers 't))  ; reuse the *ag* buffer across searches


;;------------------------------------------------------------------------------
;; Utilities
;;------------------------------------------------------------------------------
(use-package hackernews :defer t)
(use-package neotree :defer t)


(provide 'init-more-devconfigs)
