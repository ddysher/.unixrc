;;------------------------------------------------------------------------------
;; Provide window numbering mode, use M-1 ~ M-0 to switch windows
;;------------------------------------------------------------------------------
(require-package 'window-numbering)
(require 'window-numbering)

(defun window-numbering-mode-custom-hook (windows)
  "Change window number as needed."
  (if (equal current-window-conf-register ?d)
      (let ((counter 1)
            (winlen (length windows)))
        (dolist (window windows) ; assign the last two windows to 9 and 0.
          (cond
           ((equal counter (- winlen 1))  ; second-to-last window
            (window-numbering-assign window 9))
           ((equal counter winlen)        ; last window
            (window-numbering-assign window 0)))
          (cl-incf counter)))))

(window-numbering-mode t)
(setq window-numbering-auto-assign-0-to-minibuffer nil)

(provide 'init-window-numbering)
