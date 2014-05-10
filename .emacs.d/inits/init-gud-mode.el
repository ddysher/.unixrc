;;------------------------------------------------------------------------------
;; Provide gun mode, gun-mode is built-in emacs mode.  This mode enables
;; debugging python code using pdb.
;; Usage:
;;   M-x pdb, then input the script path
;;------------------------------------------------------------------------------
(require 'gud)


(defun my-gud-mode-hook ()
  (define-key gud-mode-map '[f9] 'gud-cont)
  (define-key gud-mode-map '[f10] 'gud-next)
  (define-key gud-mode-map '[f11] 'gud-step)
  (define-key gud-mode-map '[f12] 'gud-break))

(add-hook 'gud-mode-hook 'my-gud-mode-hook)


(provide 'init-gud-mode)
