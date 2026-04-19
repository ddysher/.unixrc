;;------------------------------------------------------------------------------
;; Initialize elpa package management system and use-package
;;------------------------------------------------------------------------------
(require 'package)
(require 'use-package)       ; use-package is built-in since Emacs 29.
(setq use-package-always-ensure t)

;; Package archives source
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; Load custom-file now — after package.el is loaded and initialized, but
;; before any use-package :ensure triggers package-install (which calls
;; package--save-selected-packages and could otherwise overwrite custom.el
;; with a nil list).
(load custom-file 'noerror)

;; Auto-refresh quickstart cache after installing or deleting packages.
(advice-add 'package-install :after
            (lambda (&rest _) (package-quickstart-refresh)))
(advice-add 'package-delete :after
            (lambda (&rest _) (package-quickstart-refresh)))

(provide 'init-elpa-packages)
