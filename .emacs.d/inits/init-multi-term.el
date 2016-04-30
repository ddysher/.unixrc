;;------------------------------------------------------------------------------
;; Provide multi-terminal mode, enhanced terminal mode in emacs
;;------------------------------------------------------------------------------
(require-package 'multi-term)
(require 'multi-term)


(defun term-toggle-mode ()
  "Toggle terminal between char mode and line mode."
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

;; (defun term-window-width ()
;;   "The method is defined in term.el under emacs source.  Redefined it since
;; terminal use smaller font size, while emacs still keep the same window size.
;; The 1.15 scale is based on experiment. The method should be used along with
;; the (text-scale-decrease 1), see below hook."
;;   (/ (* (window-width) 115) 100))

(defun term-mode-custom-hook ()
  (setq multi-term-program "/bin/zsh")  ; use zsh shell
  (setq multi-term-switch-after-close nil) ; don't switch to other term after close
  (setq multi-term-dedicated-window-height 20)
  (setq term-buffer-maximum-size 100000) ; increase max buffer size
  (setq-local mode-line-format (remq 'mode-line-modified mode-line-format))
  ;; Used in Mac to fix '4m' issue (wrong character in zsh). Need to run the
  ;; following command first:
  ;; $ tic -o ~/.Terminfo /Applications/Emacs.app/Contents/Resources/etc/e/eterm-color.ti
  (setenv "TERMINFO" "~/.Terminfo")
  ;; (text-scale-decrease 1)               ; use smaller size for term
  ;; NOTE: key bindings below applies to char-mode.
  ;; delete some key bindings from elpa/xx/multi-term.el.
  (setq term-bind-key-alist
        (delete '("C-p" . previous-line) term-bind-key-alist))
  (setq term-bind-key-alist
        (delete '("C-n" . next-line) term-bind-key-alist))
  (setq term-bind-key-alist
        (delete '("C-s" . isearch-forward) term-bind-key-alist))
  (setq term-bind-key-alist
        (delete '("C-r" . isearch-backward) term-bind-key-alist))
  ;; use these three bindings to switch between line mode and char mode.
  (add-to-list 'term-bind-key-alist '("C-o" . switch-to-buffer))
  (add-to-list 'term-bind-key-alist '("C-q" . term-toggle-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
  ;; use M-DEL, M-d to delete a word, previously set to 'backward-kill-word'
  ;; and "kill", which actually won't delete the word in terminal mode.
  (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("M-d"   . term-send-forward-kill-word))
  (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
  (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
  (add-to-list 'term-bind-key-alist '("M-a" . term-send-home))
  (add-to-list 'term-bind-key-alist '("M-e" . term-send-end))
  (add-to-list 'term-bind-key-alist '("M-e" . term-send-end))
  (add-to-list 'term-bind-key-alist
               '("C-<backspace>" . term-send-backward-kill-word))
  (add-to-list 'term-bind-key-alist '("C-p" . term-send-up))
  (add-to-list 'term-bind-key-alist '("C-n" . term-send-down))
  ;; NOTE: key bindings below applies to line-mode ("C-o" is not strictly
  ;; necessary, but put here for consistency.
  (define-key term-mode-map (kbd "C-o") 'switch-to-buffer)
  (define-key term-mode-map (kbd "C-q") 'term-toggle-mode)
  (define-key term-mode-map (kbd "M-[") 'multi-term-prev)
  (define-key term-mode-map (kbd "M-]") 'multi-term-next)
  (define-key term-mode-map (kbd "M-p") 'scroll-down-in-place)
  (define-key term-mode-map (kbd "M-n") 'scroll-up-in-place)
  (define-key term-mode-map (kbd "C-q") 'term-toggle-mode))


(add-hook 'term-mode-hook 'term-mode-custom-hook)


(provide 'init-multi-term)
