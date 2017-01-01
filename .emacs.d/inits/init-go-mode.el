;;------------------------------------------------------------------------------
;; Provide go mode, managed by MELPA. Several methods in go-mode requires go
;; related binaries to be properly set, e.g. gocode, godoc, godef, etc; and are
;; accessible from emacs. Tools installed:
;;   go get github.com/nsf/gocode
;;   go get github.com/tools/godep
;;   go get github.com/rogpeppe/godef
;;   go get golang.org/x/tools/cmd/guru
;;   go get golang.org/x/tools/cmd/goimports
;;
;; In addition to the above tools, we also need to include corresponding packages
;; for them to work with emacs:
;;   go-autocomplete
;;   go-eldoc
;;   go-guru
;;
;; Features:
;;   go-autocomplete provides context sensitive auto completion for Go. The
;;     feature comes with gocode (https://github.com/nsf/gocode), and requires
;;     auto-complete-mode or company-mode to be set up. gocode is client/server
;;     architecture for caching purposes (to speed up auto completion). Note,
;;     to enable auto completion on unimported packages, use:
;;       "gocode set unimported-packages true".
;;     For more detail, see github project homepage.
;;   go-eldoc shows function signature, variable definition, etc in mini-buffer
;;     at cursor point; it uses gocode as underline tool. eldoc is not exclusive
;;     to golang, it is a buffer-local minor mode that helps with looking up
;;     various documentation.
;;   go-guru: Advanced tool for Go source code analysis. For large project,
;;     go-guru can be a bit slow. For detail, see http://golang.org/s/using-guru
;;
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
(require 'go-mode)

(require-package 'go-autocomplete)
(require 'go-autocomplete)

(require-package 'go-eldoc)

(require-package 'go-guru)


(defun go-mode-custom-hook ()
  (setq gofmt-command "goimports")   ; use goimports instead of go-fmt
  (local-set-key (kbd "M-,") 'pop-tag-mark) ; same as M-*, but locally
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "C-c .") 'godef-jump-other-window))

(add-hook 'go-mode-hook 'go-mode-custom-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Load the file and we'll be able to use oracle. file-exists-p won't resolve
;; environment variable so we pull it out and concat with oracle file path.
(if (file-exists-p (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el"))
    (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el"))


(provide 'init-go-mode)
