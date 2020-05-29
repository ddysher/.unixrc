;;------------------------------------------------------------------------------
;; Provide go mode, managed by MELPA.
;;------------------------------------------------------------------------------
;; Installation
;; Several methods in go-mode requires go related binaries to be properly set,
;; e.g. gocode, godoc, godef, etc; and are accessible from emacs. Tools installed:
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
;; directly in emacs (no separate server as in go-autocomplete.
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
(require-package 'go-mode)
(require-package 'go-autocomplete)
(require-package 'go-eldoc)
(require-package 'go-guru)

(require 'go-mode)
(require 'go-autocomplete)

;; This method will be registered as a python mode hook, runs
;; every time go file is opened.
(defun go-mode-custom-hook ()
  (setq gofmt-command "goimports")   ; use goimports instead of go-fmt
  (local-set-key (kbd "M-,") 'pop-tag-mark) ; same as M-*, but locally
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c .") 'godef-jump-other-window))

(add-hook 'go-mode-hook 'go-mode-custom-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Set GO111MODULE to "off" to speed up godef.
(setenv "GO111MODULE" "off")

(provide 'init-go-mode)
