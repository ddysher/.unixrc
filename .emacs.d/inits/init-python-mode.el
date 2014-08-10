;;------------------------------------------------------------------------------
;; Provide python mode, python-mode is built-in emacs mode
;;------------------------------------------------------------------------------
(defun enable-yasnippets ()
  "Enable yasnippet minor mode and add yasnippet to autocomplete source."
  (yas-minor-mode)
  (add-to-list 'ac-sources 'ac-source-yasnippet))

(defun python-mode-custom-hook ()
  (enable-yasnippets)
  (local-set-key "\C-m" 'newline-and-indent)
  (setq python-indent-offset universal-indent-size))

(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))
(add-hook 'python-mode-hook 'python-mode-custom-hook)


(provide 'init-python-mode)
