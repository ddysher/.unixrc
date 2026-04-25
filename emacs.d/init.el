;;------------------------------------------------------------------------------
;; ~/.unixrc/emacs.d/init.el - Main Emacs Configuration Entry Point
;;
;; Loads modular init files from emacs.d/inits/.
;;
;; Structure:
;;   Bootstrap:       load-path, boot variables, package.el/use-package, shell PATH
;;   Base/interface:  editor defaults, theme/modeline, completion, windows, terminal
;;   Dev tools:       remote files, Git, Org, chat clients, coding-agent helpers
;;   Languages:       language modes, LSP registrations, file-type hooks
;;   Commands/keys:   personal commands first, then global keybindings
;;   Host-specific:   configuration loaded conditionally per host/OS
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Bootstrap - run before everything else
;;------------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-boot-configs)           ; OS/host flags, custom-file — must be first
(require 'init-elpa-packages)          ; package.el + use-package setup
(require 'init-exec-path-from-shell)   ; import shell PATH into Mac GUI Emacs

;;------------------------------------------------------------------------------
;; Base Behavior & Interface
;;------------------------------------------------------------------------------
(require 'init-preferences)
(require 'init-server)
(require 'init-theme)
(require 'init-winum)
(require 'init-terminal)
(require 'init-completion)

;;------------------------------------------------------------------------------
;; Files & Development Tools
;;------------------------------------------------------------------------------
(require 'init-org)
(require 'init-tramp)
(require 'init-magit)
(require 'init-gptel)
(require 'init-agent-tool)
(require 'init-claudemacs)
(require 'init-dev-utilities)

;;------------------------------------------------------------------------------
;; Programming Languages
;;------------------------------------------------------------------------------
(require 'init-cc-mode)
(require 'init-go-mode)
(require 'init-web-dev)
(require 'init-python-mode)
(require 'init-markdown-mode)
(require 'init-more-languages)

;;------------------------------------------------------------------------------
;; Commands & Keybindings
;;------------------------------------------------------------------------------
(require 'init-functions)
(require 'init-keys)

;;------------------------------------------------------------------------------
;; Host-specific
;;------------------------------------------------------------------------------
(when *linux*
  (require 'init-pyim))
