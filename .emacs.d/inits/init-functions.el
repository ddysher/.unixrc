;;------------------------------------------------------------------------------
;; Provide some custom functions, these are mostly bound to key short-cut.
;;------------------------------------------------------------------------------
;; Kill all buffers except current active one.
(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Scroll down line by line.
(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))

;; Scroll up line by line.
(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

;; Open a dedicated terminal and select it.
(defun multi-term-dedicated ()
  (interactive)
  (multi-term-dedicated-open)
  (multi-term-dedicated-select))

;; Search all opened buffer for a regexp pattern.
(defun search-all-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp)
  (switch-to-buffer-other-window "*Occur*"))

(defun search-buffer (regexp &optional allbufs)
  "Show all lines matching REGEXP in current buffer."
  (interactive (occur-read-primary-args))
  (occur regexp)
  (switch-to-buffer-other-window "*Occur*"))

(defun split-desktop-window ()
  (interactive)
  ;; Enable the hook only if working in the 'split mode'.
  (add-hook 'window-numbering-before-hook
            'window-numbering-mode-custom-hook)
  (split-window-below)
  (split-window-right)
  (split-window-right)
  (balance-windows)
  (enlarge-window 25) ; Bigger number results in smaller terminal area
  (other-window 3)
  (split-window-right)
  (multi-term)
  (other-window 1)
  (multi-term)
  (other-window 2))

(defun desplit-desktop-window()
  (interactive)
  (remove-hook 'window-numbering-before-hook
               'window-numbering-mode-custom-hook))


(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))


(provide 'init-functions)
