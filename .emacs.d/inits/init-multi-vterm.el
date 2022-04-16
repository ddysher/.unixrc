;;------------------------------------------------------------------------------
;; Provide multi-vterm mode, enhanced vterm-mode in emacs
;;------------------------------------------------------------------------------
(require-package 'vterm)
(require-package 'multi-vterm)
(require 'vterm)
(require 'multi-vterm)

;; Disable blinking cursor. blink-cursor-mode is a global minor mode, it's
;; tricky to disable blinking cursor only in vterm mode.
;; https://stackoverflow.com/questions/6837511/automatically-disable-a-global-minor-mode-for-a-specific-major-mode
(blink-cursor-mode -1)

(defun vterm-mode-custom-hook ()
  ;; Key bindings for veterm and multi-vterm mode.
  (define-key vterm-mode-map [f1] #'find-file)
  (define-key vterm-mode-map [f3] #'other-window)
  (define-key vterm-mode-map [f4] #'multi-vterm-dedicated-open)
  (define-key vterm-mode-map [f5] #'multi-vterm)
  (define-key vterm-mode-map [f8] #'register-to-point)
  (define-key vterm-mode-map (kbd "C-q")  #'vterm-copy-mode)
	(define-key vterm-mode-map (kbd "M-[")  #'multi-vterm-prev)
  (define-key vterm-mode-map (kbd "C-o")  #'switch-to-buffer)
  (define-key vterm-mode-map (kbd "C-q")  #'vterm-copy-mode)
	(define-key vterm-mode-map (kbd "M-[")  #'multi-vterm-prev)
	(define-key vterm-mode-map (kbd "M-]")  #'multi-vterm-next))

(add-hook 'vterm-mode-hook 'vterm-mode-custom-hook)

(provide 'init-multi-vterm)
