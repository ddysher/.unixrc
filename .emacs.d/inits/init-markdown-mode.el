;;------------------------------------------------------------------------------
;; Provide markdown mode, installed via MELPA.
;;------------------------------------------------------------------------------
(require-package 'markdown-mode)
(require 'markdown-mode)

;; Generate TOD inplace.
(defun markdown-doctoc ()
  (interactive)
  (save-buffer)
  (call-process-shell-command
   (format "doctoc %s"
           (shell-quote-argument (buffer-file-name))))
  (revert-buffer t t)
  (message "markdown table of contents updated"))

(defun markdown-live-preview ()
  ;; kill livedown server first to avoid duplicate anchors
  (interactive)
  (livedown:kill)
  (livedown:preview))

(defun markdown-custom-hook()
  (run-hooks 'prog-mode-hook)
  (local-set-key (kbd "C-c C-c d") 'markdown-doctoc)
  (local-set-key (kbd "M-p") 'scroll-down-in-place)
  (local-set-key (kbd "M-RET") 'markdown-insert-list-item)
  (local-set-key (kbd "M-<return>") 'markdown-insert-list-item)
  ;; Change 'M-p' and 'M-n' to bindings that I used to. By default, they
  ;; are bount to 'markdown-previous-link' and 'markdown-next-link'.
  (local-set-key (kbd "M-p") 'scroll-down-in-place)
  (local-set-key (kbd "M-n") 'scroll-up-in-place)
  ;; Change 'C-M-m' key binding to open livedow preview. By default, it is
  ;;  bound to 'markdown-insert-list-item'.
  (local-set-key (kbd "C-M-m") 'markdown-live-preview))

(add-hook 'markdown-mode-hook 'markdown-custom-hook)

(provide 'init-markdown-mode)
