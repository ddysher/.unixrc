;;------------------------------------------------------------------------------
;; Provide multi-vterm mode, enhanced vterm-mode in emacs
;;------------------------------------------------------------------------------
(require-package 'vterm)
(require-package 'multi-vterm)

;; Disable blinking cursor.
(blink-cursor-mode -1)

;; Defer vterm/multi-vterm loading until first terminal is opened (F4/F5).
(with-eval-after-load 'vterm
  (require 'multi-vterm) ; ensure multi-vterm-prev/next are available

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

(provide 'init-multi-vterm)
