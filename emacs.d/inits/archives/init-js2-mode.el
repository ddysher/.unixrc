;;------------------------------------------------------------------------------
;; Provide js2 mode, js2-mode is elpa managed package
;;------------------------------------------------------------------------------
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :hook ((js-mode . (lambda () (setq js-indent-level default-indent-size)))
         (js2-mode . (lambda ()
                       (setq js2-basic-offset default-indent-size)
                       (setq js2-strict-inconsistent-return-warning nil)))))

(provide 'init-js2-mode)
