;;------------------------------------------------------------------------------
;; Bootstrap configs need to be executed before loading specific configs
;;------------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-pre-configs)            ; must be called first
(require 'init-elpa-packages)          ; init elpa packages management
(require 'init-exec-path-from-shell)   ; init emacs for Mac GUI


;;------------------------------------------------------------------------------
;; General mode with configurations.
;;------------------------------------------------------------------------------
(require 'init-org)
(require 'init-gptel)
(require 'init-tramp)
(require 'init-magit)
(require 'init-winum)
(require 'init-vterm)
(require 'init-completion)

;;------------------------------------------------------------------------------
;; Promgramming language and related modes.
;;------------------------------------------------------------------------------
(require 'init-cc-mode)
(require 'init-go-mode)
(require 'init-web-dev)
(require 'init-python-mode)
(require 'init-markdown-mode)
(require 'init-more-devconfigs)

;;------------------------------------------------------------------------------
;; Custom modes, functions, etc. configurations.
;;------------------------------------------------------------------------------
(require 'init-functions)
(require 'init-keys)
(require 'init-theme)
(require 'init-preferences)

(when *linux* ;; Conditional require per host.
  (require 'init-pyim))
