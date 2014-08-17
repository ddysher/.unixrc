;;------------------------------------------------------------------------------
;; Provide yasnippet mode, managed by MELPA.  Yasnippet mode is a template
;; system for Emacs. It allows you to type an abbreviation and automatically
;; expand it into function templates.
;;
;; Configuration:
;;   Use (yas-global-mode 1) to enable yasnippet globally; or (yas-reload-all)
;;     to load snippets table, then add (yas-minor-mode) to major mode hook.
;;   Use variable yas-snippet-dirs to control which directories to load snippets
;;     table. Default directory is melpa's yasnippet installation directory, and
;;     snippets under ~/.emacs.d/snippets.
;;
;; Usage:
;;   C-o : Expand yasnippet. Originally, yas-expand is bound to TAB, but it
;;     doesn't work well with auto-complete; this file rebinds it to C-o.
;;   M-x yas-reload-all: Reload all yasnippet, useful when new snippet is added
;;     in current emacs session.
;;------------------------------------------------------------------------------
(require-package 'yasnippet)
(require 'yasnippet)


;; Load all snippets.
(yas-reload-all)
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

;; Expand yasnippet using C-o, it was bound to open-line by default in emacs.
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-o") 'yas-expand)


(provide 'init-yasnippet)
