;;------------------------------------------------------------------------------
;; Provide some custom functions, these are mostly bound to key short-cut.
;;------------------------------------------------------------------------------
;;
;; Window management
;;
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
  ;; Create erc window configuration.
  (split-desktop-window-erc)
  (other-window 1)
  (window-configuration-to-register ?e)
  ;; Create regular window configuration.
  (split-desktop-window-regular)
  (other-window 4)
  (switch-to-buffer "\*terminal<1>\*")
  (other-window 3)
  (window-configuration-to-register ?r))

(defun split-desktop-window-regular ()
  "Split desktop window for regular workflow. Clear window first,
split window, then put cursor at top left."
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
  "Split desktop window for terminal workflow. Clear window first,
split window, then put cursor at top left."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "\*scratch\*")
  (split-window-right)
  (split-window-below)
  (shrink-window-horizontally 30)
  (enlarge-window 15))

(defun split-desktop-window-erc ()
  "Split desktop window for erc workflow. Clear window first,
split window, then put cursor at top left."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "\*scratch\*")
  (split-window-right)
  (split-window-right)
  (balance-windows))

(defvar current-window-conf-register nil)

(defadvice window-configuration-to-register
    (after window-configuration-to-register-current-reg activate)
  "Restore previous window configuration."
  (setq current-window-conf-register register))

(defadvice jump-to-register
    (before jump-to-register-store-window-conf activate)
  "Store current window configuration before jumping to another register."
  (if current-window-conf-register
      (window-configuration-to-register current-window-conf-register))
  (setq current-window-conf-register register))


;;
;; Buffer management
;;
(defun kill-other-buffers ()
  "Kill all buffers except current active one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-non-term-buffers ()
  "Kill all buffers except terminals."
  (interactive)
  (let ((tokill '()))
    (dolist (buffer (buffer-list))
      (if (not (string-match "\*terminal<?>\*" (buffer-name buffer)))
          (setq tokill (nconc tokill (list buffer)))))
    (mapc 'kill-buffer tokill)))

(defun revert-all-buffers ()
  "Revert all open non-modified buffers from their respective files."
  (interactive)
  (setq count 0)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name)
                 (file-exists-p (buffer-file-name))
                 (not (buffer-modified-p)))
        (incf count)
        (revert-buffer t t t) )))
  (message "Reverted %d non-modified open files." count))

(defun search-buffer (regexp &optional allbufs)
  "Show all lines matching REGEXP in current buffer."
  (interactive (occur-read-primary-args))
  (occur regexp)
  (switch-to-buffer-other-window "*Occur*"))

(defun search-all-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp)
  (switch-to-buffer-other-window "*Occur*"))

;;
;; Misc.
;;
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


(provide 'init-functions)
