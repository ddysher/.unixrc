;;------------------------------------------------------------------------------
;; Provide js2 mode, js2-mode is elpa managed package
;;
;;------------------------------------------------------------------------------
;; Code Analyzer
;;
;; Use tern for code analysis (https://github.com/ternjs/tern).
;;------------------------------------------------------------------------------
(require-package 'js2-mode)
(require-package 'tern)
(require-package 'tern-auto-complete)
(require 'js2-mode)

(defun js-mode-custom-hook ()
  (setq js-indent-level universal-indent-size))

(defun js2-mode-custom-hook ()
  ;; "C-c C-t" is bound to (js2-mode-toggle-hide-comments) in js2-mode.
  ;; Unset it to recover the behavior to (multi-term).
  (setq js2-basic-offset universal-indent-size)
  (setq js2-strict-inconsistent-return-warning nil))

;; Use js2-mode as major mode for javascript.
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Hook js2-mode in for shell scripts running via node.js.
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Add custom hook.
(add-hook 'js-mode-hook 'js-mode-custom-hook)
(add-hook 'js2-mode-hook 'js2-mode-custom-hook)

;; Disable tern mode for now.
(when nil
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-mode t)
       (tern-ac-setup))))

(provide 'init-js2-mode)
