;;------------------------------------------------------------------------------
;; Provide window numbering mode, use M-1 ~ M-0 to switch windows
;;------------------------------------------------------------------------------
(use-package window-numbering
  :config
  (defun window-numbering-mode-custom-hook (windows)
    "Change window number as needed."
    (if (or (equal current-window-conf-register ?2)
            (equal current-window-conf-register ?3))
        (let ((counter 1)
              (winlen (length windows)))
          (dolist (window windows)
            (cond
             ((equal counter (- winlen 1))
              (window-numbering-assign window 9))
             ((equal counter winlen)
              (window-numbering-assign window 0)))
            (cl-incf counter)))))
  (window-numbering-mode t)
  (setq window-numbering-auto-assign-0-to-minibuffer nil))

(provide 'init-window-numbering)
