;;------------------------------------------------------------------------------
;; Main Emacs Configuration Entry Point
;;
;; Structure:
;;   Bootstrap:       config vars, package mgmt, shell env, custom functions, etc.
;;   Base/Interface:  editor defaults, theme, completion, workspace, terminal, etc.
;;   Notes/Tools:     org, remote files, git, chat agents, coding agent, etc.
;;   Languages:       language modes, lsp registrations, file-type hooks, etc.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Bootstrap - run before everything else
;;------------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-config-vars)
(require 'init-functions)
(require 'init-elpa-packages)
(require 'init-exec-path-from-shell)

;;------------------------------------------------------------------------------
;; Base Behavior & Interface
;;------------------------------------------------------------------------------
(require 'init-defaults)
(require 'init-global-keys)
(require 'init-theme)
(require 'init-dirvish)
(require 'init-terminal)
(require 'init-completion)
(require 'init-workspaces)

;;------------------------------------------------------------------------------
;; Notes & Development Tools
;;------------------------------------------------------------------------------
(require 'init-org)
(require 'init-pyim)
(require 'init-tramp)
(require 'init-magit)
(require 'init-gptel)
(require 'init-agent-tool)
(require 'init-agent-packages)

;;------------------------------------------------------------------------------
;; Programming Languages
;;------------------------------------------------------------------------------
(require 'init-cc-mode)
(require 'init-go-mode)
(require 'init-web-mode)
(require 'init-python-mode)
(require 'init-markdown-mode)
(require 'init-language-modes)
