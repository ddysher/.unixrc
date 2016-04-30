;;------------------------------------------------------------------------------
;; Provide go mode, managed by MELPA.  Serveral methods in go-mode requires go
;; related binaries be properly set, e.g. godoc, dodef, etc.  For my configs,
;; all these binaries are installed under ~/code/workspace/bin. Go environment
;; variables are set in ~/.unixrc/.zshrc. Tools installed:
;;   go get github.com/nsf/gocode
;;   go get github.com/tools/godep
;;   go get github.com/rogpeppe/godef
;;   go get golang.org/x/tools/cmd/goimports
;;   go get golang.org/x/tools/cmd/oracle
;;
;; Feature:
;;   go-autocomplete: Provide context sensitive auto completion for Go.
;;     - https://github.com/nsf/gocode/
;;     The feature needs auto-complete be set up first; also, it needs gocode
;;     installed on the system (gocode in PATH), and needs to load a file under
;;     emacs/go-autocomplete.el (optional if installed from elpa).
;;   go-eldoc: Show function signature, variable definition in mini-buffer,
;;     use gocode as underline tool.
;;   go-oracle: Advanced tool for source code analysis
;;     - https://docs.google.com/document/d/1SLk36YRjjMgKqe490mSRzOPYEDe0Y_WQNRv-EiFYUyw/view
;;     go-oracle only works with go1.6 vendor support, e.g. it won't honor Godeps
;;
;; Usage:
;;   M-x godoc  ; give a package name, show docs in view-mode
;;   M-x gofmt  ; format current go buffer
;;   M-.        ; jump to the definition of symbol
;;   M-*        ; jump back
;;   C-c C-a    ; add a package to import (usually we import package via goimports,
;;              ; but this can be usefull when wanting to use gocode, as gocode
;;              ; can't autocomplete if the package is not impoted)
;;------------------------------------------------------------------------------
(require-package 'go-mode)
(require 'go-mode)

(require-package 'go-autocomplete)
(require 'go-autocomplete)

(require-package 'go-eldoc)


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
