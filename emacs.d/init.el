;;------------------------------------------------------------------------------
;; Bootstrap configs need to be executed before loading specific configs
;;------------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-pre-configs)            ; must be called first
(require 'init-elpa-packages)          ; init elpa packages management
(require 'init-exec-path-from-shell)   ; init emacs for Mac GUI


;;------------------------------------------------------------------------------
;; Load configs for features and modes
;;------------------------------------------------------------------------------
;; General mode with configurations.
(require 'init-org)
(require 'init-gptel)
(require 'init-tramp)
(require 'init-completion)
(require 'init-magit-mode)
(require 'init-winum-mode)
(require 'init-multi-vterm)

;; Initialize language related modes that require configurations.
(require 'init-cc-mode)
(require 'init-go-mode)
(require 'init-web-dev)
(require 'init-python-mode)
(require 'init-markdown-mode)
(require 'init-more-devconfigs)

;; My custom mode, functions, etc.
(require 'init-functions)
(require 'init-keys)
(require 'init-theme)
(require 'init-custom)

;; Conditional require per host.
(when *linux*
  (require 'init-pyim))
