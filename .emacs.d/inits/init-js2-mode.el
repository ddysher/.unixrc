;;------------------------------------------------------------------------------
;; Provide javascript mode, javascript-mode is built-in emacs mode
;;------------------------------------------------------------------------------
(require-package 'js2-mode)
(require-package 'tern)
(require-package 'tern-auto-complete)
(require 'js2-mode)


(defun js2-mode-custom-hook ()
  ;; (tern-mode t)
  (setq js2-basic-offset universal-indent-size)
  ;; It's perfectly legal to have return and value-return in the same function.
  (setq js2-strict-inconsistent-return-warning nil))
(defun js-mode-custom-hook ()
  (setq js-indent-level universal-indent-size))

;; Use js2-mode as major mode for javascript.
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Hook js2-mode in for shell scripts running via node.js.
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Add custom hook.
(add-hook 'js2-mode-hook 'js2-mode-custom-hook)
(add-hook 'js-mode-hook 'js-mode-custom-hook)

;; (eval-after-load 'tern
;;    '(progn
;;       (require 'tern-auto-complete)
;;       (tern-ac-setup)))


(provide 'init-js2-mode)
