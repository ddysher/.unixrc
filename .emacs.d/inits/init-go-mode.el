;;------------------------------------------------------------------------------
;; Provide go mode, managed by MELPA.
;;------------------------------------------------------------------------------
;; Install go-mode with golang tools and other emacs packages.
;;
;; Tools:
;;   go install golang.org/x/tools/cmd/goimports@latest
;;   go install golang.org/x/tools/gopls@latest
;;
;; Packages:
;;   M-x package-install go-mode
;;   M-x package-install lsp-mode
;;   M-x package-install company
;;------------------------------------------------------------------------------
;; How it works
;;
;; Most of the features in Go dev environment are provided by lsp-mode, including
;; code navigation, code completion (with company), etc.
;;------------------------------------------------------------------------------
;; Usage:
;;   M-.        ; jump to the definition of symbol
;;   M-,        ; jump back
;;   M-x gofmt  ; format current go buffer (will be called when saving file)
;;   C-c C-a    ; add a package to import
;;   C-c C-f ?  ; quite a few commands have C-c C-f prefix, e.g. 'a' for jumping
;;              ; to function argument, 'r' for jumping to function return value
;;------------------------------------------------------------------------------
(require-package 'go-mode)

(require 'go-mode)

;; This method will be registered as a go mode hook, runs every time
;; a go file is opened.
(defun go-mode-custom-hook ()
  (setq gofmt-command "goimports")   ; use goimports instead of go-fmt
  (local-set-key (kbd "M-,") 'pop-tag-mark) ; same as M-*, but locally
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (local-set-key (kbd "M-/") 'lsp-find-references))

(add-hook 'go-mode-hook 'go-mode-custom-hook)
(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'init-go-mode)


;; ------------------- Historical Configuration --------------------------------
;; -----------------------------------------------------------------------------
;; Installation & Usage V1 (Deprecated, use gopls & lsp-mode instead)
;;------------------------------------------------------------------------------
;; Installation
;; Several methods in go-mode requires go related binaries to be properly set,
;; e.g. gocode, godoc, godef, etc; and are accessible from emacs.
;; Tools installed:
;;   go get -u github.com/nsf/gocode
;;   go get -u github.com/tools/godep
;;   go get -u github.com/rogpeppe/godef
;;   go get -u golang.org/x/tools/cmd/guru
;;   go get -u golang.org/x/tools/cmd/goimports
;;
;; In addition to the above tools, we also need to include corresponding packages
;; for them to work with emacs:
;;   go-autocomplete
;;   go-eldoc
;;   go-guru
;;
;;------------------------------------------------------------------------------
;; Autocompletion
;;
;; Package "go-autocomplete" provides context sensitive auto completion for Go.
;; The feature comes with gocode (https://github.com/nsf/gocode), and requires
;; auto-complete-mode or company-mode to be set up. gocode is client/server
;; architecture for caching purposes (to speed up auto completion). Note, to
;; enable auto completion on unimported packages, use:
;;   "gocode set unimported-packages true".
;; For more detail, see github project homepage https://github.com/nsf/gocode.
;;
;; Package go-eldoc shows function signature, variable definition, etc in mini
;; buffer at cursor point; it uses gocode as underline tool. eldoc is not exclusive
;; to golang, it is a buffer-local minor mode that helps with looking up various
;; documentation.
;;
;; How it works. gocode comes with a go-autocomplete.el package, which calls gocode
;; process command for autocompletion suggestion. A separate gocode process is
;; running in the host system, e.g.
;;   gocode -s -sock unix -addr 127.0.0.1:37373
;; which accepts autocompletion request and send back response.
;;
;;------------------------------------------------------------------------------
;; Code Jump
;;
;; Code Jump is done through the tool "godef", which finds symbol information in
;; Go source. The tool is included in "go-mode". go-mode invokes the godef binary
;; directly in emacs (no separate server as in go-autocomplete).
;;
;;------------------------------------------------------------------------------
;; Code Analysis
;;
;; Package go-guru is and advanced tool for Go source code analysis. For large
;; project, go-guru can be a bit slow. For detail, see http://golang.org/s/using-guru
;;
;;------------------------------------------------------------------------------
;; Usage:
;;   M-.        ; jump to the definition of symbol
;;   M-*        ; jump back
;;   M-x godoc  ; give a package name, show docs in view-mode
;;   M-x gofmt  ; format current go buffer (will be called when saving file)
;;   M-x go-guru-set-scope  ; set scope for go-guru analysis
;;   M-x go-guru-referrers  ; show all refs to thing denoted by selected identifier
;;   C-c C-a    ; add a package to import
;;   C-c C-f ?  ; quite a few commands have C-c C-f prefix, e.g. 'a' for jumping
;;              ; to function argument, 'r' for jumping to function return value
;;------------------------------------------------------------------------------
