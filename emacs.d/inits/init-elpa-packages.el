;;------------------------------------------------------------------------------
;; Initialize elpa package management system and use-package.
;;------------------------------------------------------------------------------

;; The package "use-package" is built-in since Emacs 29.
(require 'package)
(require 'use-package)

;; Package archives source
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Always install missing packages.
(setq use-package-always-ensure t)

;; Initializes the package management system, which loads the installed
;; packages list and makes them available. This must be called since we
;; disabled package initialization in early-init.el.
(package-initialize)

;; Load path theming as early as possible after package initialization.
(use-package no-littering
  :demand t)

;; Keep custom-file explicit and repo-local so it can be checked in.
;; It is loaded later in init-elpa-packages, after package initialization.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load custom-file now — after package.el is loaded and initialized, but
;; before any use-package :ensure triggers package-install (which calls
;; package--save-selected-packages and could otherwise overwrite custom.el
;; with a nil list).
(load custom-file 'noerror)

(provide 'init-elpa-packages)
