;;------------------------------------------------------------------------------
;; Provide multi-terminal mode, enhanced terminal mode in emacs
;;------------------------------------------------------------------------------
(require-package 'multi-term)
(require 'multi-term)


(defun term-toggle-mode ()
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun term-mode-custom-hook ()
  (setq multi-term-program "/bin/zsh")  ; use zsh shell
  (setq multi-term-switch-after-close nil) ; do not swith to other terminal when close
  (setq multi-term-dedicated-window-height 20)
  (setq term-buffer-maximum-size 10000) ; increase max buffer size
  (setenv "TERMINFO" "~/.Terminfo")     ; what's this?
  (text-scale-decrease 1)
  (define-key term-mode-map [f9] 'term-toggle-mode)
  ;; delete some key bindings from elpa/xx/multi-term.el, which will give
  ;; back their original meaning from term.el.
  (setq term-bind-key-alist
        (delete '("C-p" . previous-line) term-bind-key-alist))
  (setq term-bind-key-alist
        (delete '("C-n" . next-line) term-bind-key-alist))
  (setq term-bind-key-alist
        (delete '("C-s" . isearch-forward) term-bind-key-alist))
  (setq term-bind-key-alist
        (delete '("C-r" . isearch-backward) term-bind-key-alist))
  ;; use these three bindings to switch between line mode and char mode
  (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
  (add-to-list 'term-bind-key-alist '([f9] . term-toggle-mode))
  ;; everything below is key bindings in term-char-mode
  ;; use M-DEL, M-d to delete a word, previously set to 'backward-kill-word'
  ;; and "kill", which actually won't delete the word in terminal mode.
  (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("M-d"   . term-send-forward-kill-word))
  (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
  (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
  (add-to-list 'term-bind-key-alist '("M-a" . term-send-home))
  (add-to-list 'term-bind-key-alist '("M-e" . term-send-end))
  (add-to-list 'term-bind-key-alist '("M-e" . term-send-end))
  (add-to-list 'term-bind-key-alist '("C-<backspace>" .
                                      term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("C-p" . term-send-up))
  (add-to-list 'term-bind-key-alist '("C-n" . term-send-down))
  (add-to-list 'term-bind-key-alist '("C-y" . term-paste)))


(add-hook 'term-mode-hook 'term-mode-custom-hook)


(provide 'init-multi-term)
