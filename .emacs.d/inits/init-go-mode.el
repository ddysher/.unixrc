;;------------------------------------------------------------------------------
;; Provide go mode, managed by MELPA.  Serveral methods in go-mode needs go
;; related binaries be properly set, e.g. godoc, dodef, etc.  For my configs,
;; all these binaries are installed under ~/code/source/go-workspace/bin, refer
;; to ~/.unixrc/install.sh. The go environment variables are set in .zshrc.
;;
;; Feature:
;;   go-autocomplete: Provide context sensitive auto completion for Go. The
;;     feature needs auto-complete be set up first; also, it needs gocode
;;     installed on the system (gocode in PATH).
;;   go-eldoc: Show function signature, variable definition in mini-buffer,
;;     use gocode as underline tool.
;;
;; Usage:
;;   M-x godoc  ; give a package name, show docs in view-mode.
;;   M-x gofmt  ; format current go buffer.
;;   M-.        ; jump to the definition of symbol.
;;   M-*        ; jump back.
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
  (local-set-key (kbd "C-c .") 'godef-jump-other-window)
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Workaround of a bug in godef (cannot jump if working in forked project).
  (if (and (string-match "/ddysher/kubernetes/" (buffer-file-name))
           (not (member "[ddysher] " 'mode-line-format)))
      (add-to-list 'mode-line-format "[ddysher] "))
  (if (and (string-match "/GoogleCloudPlatform/kubernetes/" (buffer-file-name))
           (not (member "[Google] " 'mode-line-format)))
      (add-to-list 'mode-line-format "[Google] ")))

(add-hook 'go-mode-hook 'go-mode-custom-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)


(provide 'init-go-mode)
