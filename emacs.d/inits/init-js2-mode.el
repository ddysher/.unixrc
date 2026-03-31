;;------------------------------------------------------------------------------
;; Provide js2 mode, js2-mode is elpa managed package
;;
;;------------------------------------------------------------------------------
;; Code Analyzer
;;
;; Use tern for code analysis (https://github.com/ternjs/tern).
;;------------------------------------------------------------------------------
(require-package 'js2-mode)

;; auto-mode-alist entries work before js2-mode is loaded.
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(defun js-mode-custom-hook ()
  (setq js-indent-level universal-indent-size))

(defun js2-mode-custom-hook ()
  (setq js2-basic-offset universal-indent-size)
  (setq js2-strict-inconsistent-return-warning nil))

(add-hook 'js-mode-hook 'js-mode-custom-hook)
(add-hook 'js2-mode-hook 'js2-mode-custom-hook)

(provide 'init-js2-mode)
