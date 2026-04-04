;;------------------------------------------------------------------------------
;; Provide multi-vterm mode, enhanced vterm-mode in emacs
;;------------------------------------------------------------------------------
(use-package vterm
  :defer t
  :init
  (blink-cursor-mode -1)
  :config
  (defun vterm-mode-custom-hook ()
    (define-key vterm-mode-map [f1] #'find-file)
    (define-key vterm-mode-map [f3] #'other-window)
    (define-key vterm-mode-map [f4] #'multi-vterm-dedicated-open)
    (define-key vterm-mode-map [f5] #'multi-vterm)
    (define-key vterm-mode-map [f8] #'register-to-point)
    (define-key vterm-mode-map (kbd "C-o")  #'switch-to-buffer)
    (define-key vterm-mode-map (kbd "C-q")  #'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "M-[")  #'multi-vterm-prev)
    (define-key vterm-mode-map (kbd "M-]")  #'multi-vterm-next))
  (add-hook 'vterm-mode-hook 'vterm-mode-custom-hook))

(use-package multi-vterm
  :after vterm
  :commands (multi-vterm multi-vterm-dedicated-open multi-vterm-prev multi-vterm-next))

(provide 'init-multi-vterm)
