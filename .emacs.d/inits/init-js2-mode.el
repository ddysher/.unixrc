;;------------------------------------------------------------------------------
;; Provide javascript mode, javascript-mode is built-in emacs mode
;;------------------------------------------------------------------------------
(require-package 'js2-mode)
(require 'js2-mode)


(defun js2-mode-custom-hook ()
  (setq js2-basic-offset 2))
(defun js-mode-custom-hook ()
  (setq js-indent-level 2))


;; Use js2-mode as major mode for javascript.
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Hook js2-mode in for shell scripts running via node.js.
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;; Add custom hook.
(add-hook 'js2-mode-hook 'js2-mode-custom-hook)
(add-hook 'js-mode-hook 'js-mode-custom-hook)


(provide 'init-js2-mode)
