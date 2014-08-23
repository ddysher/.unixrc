;;------------------------------------------------------------------------------
;; Provide some custom functions, these are mostly bound to key short-cut.
;;------------------------------------------------------------------------------
(defun start-workspace ()
  "Start custom workspace, i.e. window configuration."
  (interactive)
  (add-hook 'window-numbering-before-hook
            'window-numbering-mode-custom-hook)
  ;; Create terminal window configuration.
  (split-desktop-window-terminal)
  (other-window 2)
  (multi-term)
  (multi-term)
  (switch-to-buffer "\*terminal<1>\*")
  (window-configuration-to-register ?t)
  ;; Create regular window configuration.
  (split-desktop-window-regular)
  (other-window 4)
  (switch-to-buffer "\*terminal<1>\*")
  (other-window 3)
  (window-configuration-to-register ?r))

;; Kill all buffers except current active one.
(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Kill all buffers except terminals.
(defun kill-non-term-buffers ()
  (interactive)
  (let ((tokill '()))
    (dolist (buffer (buffer-list))
      (if (not (string-match "\*terminal<?>\*" (buffer-name buffer)))
          (setq tokill (nconc tokill (list buffer)))))
    (mapc 'kill-buffer tokill)))

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

(defun split-desktop-window-regular ()
  "Split desktop window for regular workflow. Clear window first, split window
then put cursor at top left."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "\*scratch\*")
  ;; Splite window
  (split-window-below)
  (split-window-right)
  (split-window-right)
  (other-window 3)
  (split-window-right)
  (split-window-right)
  ;; Adjust window size
  (balance-windows)
  (shrink-window 25)                    ; shrink bottom windows
  (shrink-window-horizontally 10)       ; shrink bottom left window
  (other-window 2)
  (shrink-window-horizontally 10)       ; shrink bottom right window
  (other-window 1))

(defun split-desktop-window-terminal ()
  "Split desktop window for terminal workflow. Clear window first, split window
then put cursor at top left."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "\*scratch\*")
  (split-window-right)
  (split-window-below)
  (shrink-window-horizontally 30))

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
