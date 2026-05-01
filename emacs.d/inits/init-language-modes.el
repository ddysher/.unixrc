;;------------------------------------------------------------------------------
;; Lightweight language modes that need little or no configuration.
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
;; File type associations
;;------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.gitmodules$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\BUILD$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.defs$" . conf-mode))

;;------------------------------------------------------------------------------
;; Data and markup formats
;;------------------------------------------------------------------------------
(use-package yaml-mode
  :defer t
  :hook (yaml-mode . (lambda () (run-hooks 'prog-mode-hook))))  ; not a prog-mode child
(use-package thrift :defer t)
(use-package protobuf-mode
  :defer t
  :hook (protobuf-mode . (lambda () (setq c-basic-offset default-indent-size))))
(use-package dockerfile-mode :defer t)
(use-package wsd-mode :defer t)

;;------------------------------------------------------------------------------
;; Scripting languages
;;------------------------------------------------------------------------------
(defun sh-mode-custom-hook ()
  (setq sh-indentation default-indent-size)
  (setq sh-basic-offset default-indent-size))
(add-hook 'sh-mode-hook 'sh-mode-custom-hook)

(use-package lua-mode
  :defer t
  :hook (lua-mode . (lambda () (setq lua-indent-level default-indent-size))))

(use-package php-mode :defer t)

(use-package matlab-mode
  :defer t
  :mode ("\\.m\\'" . matlab-mode))

;;------------------------------------------------------------------------------
;; System languages
;;------------------------------------------------------------------------------
(defun java-mode-custom-hook ()
  (setq c-basic-offset default-indent-size)
  (setq tab-width default-indent-size)
  (local-set-key "\C-m" 'newline-and-indent)) ; RET indents the new line

(add-hook 'java-mode-hook 'java-mode-custom-hook)

(use-package scala-mode :defer t)
(use-package sbt-mode :defer t)
(use-package rust-mode :defer t)

;;------------------------------------------------------------------------------
;; Infrastructure config
;;------------------------------------------------------------------------------
(use-package apache-mode
  :defer t
  :mode ("/etc/apache2/.*" . apache-mode))
(use-package nginx-mode
  :defer t
  :mode ("/etc/nginx/.*" . nginx-mode))

(provide 'init-language-modes)
