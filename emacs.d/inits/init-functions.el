;;------------------------------------------------------------------------------
;; Custom interactive functions and utilities.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Buffer management
;;------------------------------------------------------------------------------
(defun kill-other-buffers ()
  "Kill all buffers except the current buffer."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun text-scale-increase-all-buffers ()
  "Increase text scale in all buffers by one step."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (text-scale-increase 1)))
  (message "Increased all buffers' size"))

(defun text-scale-decrease-all-buffers ()
  "Decrease text scale in all buffers by one step."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (text-scale-increase -1)))
  (message "Decreased all buffers' size"))

(defun text-scale-reset-all-buffers ()
  "Reset text scale in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (text-scale-increase 0)))
  (message "Reset all buffers' size"))

(defun laura/tabspaces-kill-buffers-close-workspace-confirm ()
  "Ask for confirmation before killing the current workspace's buffers and closing it."
  (interactive)
  (when (yes-or-no-p "Kill all buffers in this workspace and close it? ")
    (tabspaces-kill-buffers-close-workspace)))

;;------------------------------------------------------------------------------
;; Editing
;;------------------------------------------------------------------------------
(defun scroll-down-in-place (n)
  "Scroll down N lines while moving point by N lines."
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun scroll-up-in-place (n)
  "Scroll up N lines while moving point by N lines."
  (interactive "p")
  (next-line n)
  (scroll-up n))

;;------------------------------------------------------------------------------
;; Utilities
;;------------------------------------------------------------------------------
(require 'ansi-color)
(defun display-ansi-colors ()
  "Render ANSI color escape sequences in the current buffer.
Useful for log files or pasted shell output containing escape sequences."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun sudo-edit (&optional arg)
  "Edit current file as root.
With prefix ARG, or when the current buffer is not visiting a file, prompt for a
file to open as root."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun show-emacs-pid ()
  "Show the current Emacs process ID."
  (interactive)
  (message "emacs pid: %s" (emacs-pid)))

(provide 'init-functions)
