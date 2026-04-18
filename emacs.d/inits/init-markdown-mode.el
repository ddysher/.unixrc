;;------------------------------------------------------------------------------
;; Provide markdown mode, installed via MELPA.
;;------------------------------------------------------------------------------

;; Livedown: live markdown preview in browser.
;; Requires 'livedown' npm command: npm install -g livedown
(use-package livedown
  :vc (:url "https://github.com/shime/emacs-livedown")
  :commands (livedown:preview livedown:kill)
  :custom
  (livedown:autostart nil)
  (livedown:open t)
  (livedown:port 1337))

(use-package markdown-mode
  :custom
  (markdown-command "pandoc")
  :hook (markdown-mode . markdown-custom-hook))

;; Generate TOC inplace.
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

(provide 'init-markdown-mode)
