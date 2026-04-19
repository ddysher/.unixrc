;;------------------------------------------------------------------------------
;; ~/.unixrc/emacs.d/init.el - Main Emacs Configuration Entry Point
;;
;; Loads modular init files from emacs.d/inits/.
;;
;; Structure:
;;   init-pre-configs            - Early setup: custom-file, OS/host flags
;;   init-elpa-packages          - Package management (ELPA/use-package)
;;   init-exec-path-from-shell   - Sync PATH from shell (Mac GUI fix)
;;
;;   General:      org, gptel, tramp, magit, winum, vterm, completion
;;   Languages:    cc, go, web, python, markdown, misc dev configs
;;   Customization: functions, keybindings, theme, editor preferences
;;
;; Host-specific:
;;   init-pyim loaded only on Linux (Chinese input method)
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Bootstrap - must run before everything else
;;------------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-pre-configs)            ; OS/host flags, custom-file — must be first
(require 'init-elpa-packages)          ; package.el + use-package setup
(require 'init-exec-path-from-shell)   ; import shell PATH into Mac GUI Emacs


;;------------------------------------------------------------------------------
;; General modes
;;------------------------------------------------------------------------------
(require 'init-org)
(require 'init-gptel)
(require 'init-tramp)
(require 'init-magit)
(require 'init-winum)
(require 'init-vterm)
(require 'init-completion)


;;------------------------------------------------------------------------------
;; Programming language modes
;;------------------------------------------------------------------------------
(require 'init-cc-mode)
(require 'init-go-mode)
(require 'init-web-dev)
(require 'init-python-mode)
(require 'init-markdown-mode)
(require 'init-more-devconfigs)


;;------------------------------------------------------------------------------
;; Customization - keybindings, theme, preferences, helper functions
;;------------------------------------------------------------------------------
(require 'init-functions)
(require 'init-keys)
(require 'init-theme)
(require 'init-preferences)


;;------------------------------------------------------------------------------
;; Host-specific
;;------------------------------------------------------------------------------
(when *linux*
  (require 'init-pyim))
