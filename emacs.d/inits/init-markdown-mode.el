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

(defgroup markdown-glow nil
  "Preview Markdown with glow inside Emacs."
  :group 'markdown)

(defcustom markdown-glow-command "glow"
  "Command used to render Markdown previews."
  :type 'string
  :group 'markdown-glow)

(defcustom markdown-glow-extra-arguments '("--pager")
  "Extra arguments passed to glow before the Markdown source."
  :type '(repeat string)
  :group 'markdown-glow)

(defcustom markdown-glow-buffer-name "*markdown-glow*"
  "Base name used for glow preview buffers."
  :type 'string
  :group 'markdown-glow)

(defvar-local markdown-glow-temp-file nil
  "Temporary file used to render the current glow preview buffer.")

(defvar-local markdown-glow-source-buffer nil
  "Source Markdown buffer for the current glow preview buffer.")

(define-minor-mode markdown-glow-preview-mode
  "Minor mode for Markdown previews rendered by glow in vterm buffers."
  :lighter " Glow")

(defun markdown-glow--width (source-buffer)
  (with-current-buffer source-buffer
    (max 60 (- (window-body-width (or (get-buffer-window source-buffer)
                                      (selected-window)))
               4))))

(defun markdown-glow--preview-buffer (source-buffer)
  (get-buffer-create
   (format "%s<%s>" markdown-glow-buffer-name
           (buffer-name source-buffer))))

(defun markdown-glow--temp-file (source-buffer)
  (or (buffer-local-value 'markdown-glow-temp-file source-buffer)
      (with-current-buffer source-buffer
        (setq-local markdown-glow-temp-file
                    (make-temp-file "markdown-glow-" nil ".md")))))

(defun markdown-glow--write-temp-file (source-buffer)
  (let ((temp-file (markdown-glow--temp-file source-buffer)))
    (with-temp-file temp-file
      (insert-buffer-substring source-buffer))
    temp-file))

(defun markdown-glow--command (source-buffer)
  (unless (executable-find markdown-glow-command)
    (user-error "Cannot find `%s` in exec-path" markdown-glow-command))
  (format "%s %s --width %s %s"
          (shell-quote-argument markdown-glow-command)
          (mapconcat #'shell-quote-argument
                     markdown-glow-extra-arguments
                     " ")
          (number-to-string (markdown-glow--width source-buffer))
          (shell-quote-argument
           (markdown-glow--write-temp-file source-buffer))))

(defun markdown-glow--ensure-vterm-buffer (preview-name)
  (require 'vterm)
  (let ((preview-buffer (get-buffer preview-name)))
    (when (buffer-live-p preview-buffer)
      (with-current-buffer preview-buffer
        (unless (derived-mode-p 'vterm-mode)
          (kill-buffer preview-buffer)
          (setq preview-buffer nil))))
    (or preview-buffer
        (let ((vterm-buffer-name preview-name))
          (vterm vterm-buffer-name)))))

(defun markdown-glow--render (source-buffer preview-buffer)
  (let* ((command (markdown-glow--command source-buffer))
         (vterm-buffer
          (markdown-glow--ensure-vterm-buffer (buffer-name preview-buffer))))
    (with-current-buffer vterm-buffer
      (markdown-glow-preview-mode 1)
      (setq-local markdown-glow-source-buffer source-buffer)
      (setq-local revert-buffer-function #'markdown-glow-preview-refresh)
      (setq-local truncate-lines t)
      (vterm-clear)
      (vterm-send-string command)
      (vterm-send-return))
    vterm-buffer))

(defun markdown-glow-preview ()
  "Render the current Markdown buffer with glow inside Emacs."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (preview-buffer (markdown-glow--preview-buffer source-buffer)))
    (pop-to-buffer (markdown-glow--render source-buffer preview-buffer))))

(defun markdown-glow-preview-refresh (&rest _)
  "Refresh the current glow preview buffer."
  (interactive)
  (unless (buffer-live-p markdown-glow-source-buffer)
    (user-error "Source Markdown buffer is no longer available"))
  (markdown-glow--render markdown-glow-source-buffer (current-buffer)))

(use-package markdown-mode
  :hook (markdown-mode . markdown-custom-hook)
  :bind (:map markdown-mode-map
              ("C-c C-c p" . markdown-glow-preview)
              ("C-c C-c g" . markdown-glow-preview)))

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
  ;; Keep the old browser preview available on demand.
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
  ;; Use glow for native in-Emacs preview.
  (local-set-key (kbd "C-M-m") 'markdown-glow-preview))

(provide 'init-markdown-mode)
