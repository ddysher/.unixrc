;;------------------------------------------------------------------------------
;; Main Emacs Configuration Entry Point
;;
;; Structure:
;;   Bootstrap:       load-path, boot variables, package mgmt, shell env, etc.
;;   Base/Interface:  editor defaults, theme, completion, windows, terminal, etc.
;;   Notes/Tools:     org, remote files, git, chat agents, coding agent, etc.
;;   Languages:       language modes, lsp registrations, file-type hooks, etc.
;;   Commands/Keys:   personal commands, global keybindings, etc.
;;   Host-specific:   configuration loaded conditionally per host/os, etc.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Bootstrap - run before everything else
;;------------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-boot-configs)
(require 'init-elpa-packages)
(require 'init-exec-path-from-shell)

;;------------------------------------------------------------------------------
;; Base Behavior & Interface
;;------------------------------------------------------------------------------
(require 'init-defaults)
(require 'init-theme)
(require 'init-winum)
(require 'init-dirvish)
(require 'init-terminal)
(require 'init-completion)

;;------------------------------------------------------------------------------
;; Notes & Development Tools
;;------------------------------------------------------------------------------
(require 'init-org)
(require 'init-tramp)
(require 'init-magit)
(require 'init-gptel)
(require 'init-agent-tool)
(require 'init-claudemacs)

;;------------------------------------------------------------------------------
;; Programming Languages
;;------------------------------------------------------------------------------
(require 'init-cc-mode)
(require 'init-go-mode)
(require 'init-web-mode)
(require 'init-python-mode)
(require 'init-markdown-mode)
(require 'init-language-modes)

;;------------------------------------------------------------------------------
;; Commands & Keybindings
;;------------------------------------------------------------------------------
(require 'init-functions)
(require 'init-global-keys)

;;------------------------------------------------------------------------------
;; Host-specific
;;------------------------------------------------------------------------------
(when *linux*
  (require 'init-pyim))
