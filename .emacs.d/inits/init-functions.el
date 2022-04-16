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
  ;; Create simple window configuration.
  (split-desktop-window-regular-1)
  (other-window 1)
  (window-configuration-to-register ?s)
  ;; Create terminal window configuration.
  (split-desktop-window-terminal)
  (other-window 2)
  (multi-vterm)
  (multi-vterm)
  (multi-vterm)
  (multi-vterm-next)
  (window-configuration-to-register ?t)
  ;; Create regular 2-window configuration (with mini-terminal).
  (split-desktop-window-regular-2)
  (other-window 3)
  (switch-to-buffer "\*vterminal<1>\*")
  (other-window 2)
  (window-configuration-to-register ?g)
  ;; Create regular 3-window configuration (with mini-terminal).
  (split-desktop-window-regular-3)
  (other-window 4)
  (switch-to-buffer "\*vterminal<1>\*")
  (other-window 3)
  (window-configuration-to-register ?r))

(defun split-desktop-window-regular-1 ()
  "regular-1 splits to two windows"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (switch-to-buffer "\*scratch\*"))

(defun split-desktop-window-regular-2 ()
  "regular-2 splits main workspace to two windows with mini-terminal."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "\*scratch\*")
  ;; Splite window
  (split-window-below)
  (split-window-right)
  (other-window 2)
  (split-window-right)
  (split-window-right)
  ;; Adjust window size
  (balance-windows)
  ;; Up to this point, lower windows takes up 50% space.
  (shrink-window (/ (* (window-body-height) 7) 10)) ; shrink bottom windows to 3/10 of its current size
  (shrink-window-horizontally (/ (* (window-width) 5) 10)) ; shrink bottom left window to half of its current size
  (other-window 2)
  (shrink-window-horizontally (/ (* (window-width) 5) 10)) ; shrink bottom right window to half of its current size
  (other-window 1))

(defun split-desktop-window-regular-3 ()
  "regular-3 splits main workspace to three windows with mini-terminal"
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
  ;; Up to this point, lower windows takes up 50% space.
  (shrink-window (/ (* (window-body-height) 7) 10)) ; shrink bottom windows to 3/10 of its current size
  (shrink-window-horizontally (/ (* (window-width) 5) 10)) ; shrink bottom left window to half of its current size
  (other-window 2)
  (shrink-window-horizontally (/ (* (window-width) 5) 10)) ; shrink bottom right window to half of its current size
  (other-window 1))

(defun split-desktop-window-terminal ()
  "Split desktop window for terminal workflow. Clear window first,
split window, then put cursor at top left."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "\*scratch\*")
  (split-window-right)
  (split-window-below)
  (shrink-window-horizontally (/ (* (window-width) 3) 10))) ; shrink left window to 7/10 of its current size

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
      (if (not (string-match "\*vterminal<?>\*" (buffer-name buffer)))
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
        (cl-incf count)
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

(defun text-scale-increase-all-buffers ()
  "Increate all buffers' size by one"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (text-scale-increase 1)))
  (message "Increased all buffers' size"))

(defun text-scale-decrease-all-buffers ()
  "Increate all buffers' size by one"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (text-scale-increase -1)))
  (message "Decreased all buffers' size"))

(defun text-scale-reset-all-buffers ()
  "Increate all buffers' size by one"
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (text-scale-increase 0)))
  (message "Reset all buffers' size"))


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

(defun show-emacs-pid ()
  (interactive)
  (message "emacs pid: %s" (emacs-pid)))

(defun sudo-edit (&optional arg)
  ;; Edit currently visited file as root, or open a new file as root if current
  ;; buffer does not associate with a file.
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'init-functions)
