;;------------------------------------------------------------------------------
;; Provide window numbering mode, use M-1 ~ M-0 to switch windows
;;------------------------------------------------------------------------------
(require-package 'window-numbering)
(require 'window-numbering)


(defun window-numbering-mode-custom-hook (windows)
  "Change window number as needed."
  (if (and (>= (length windows) 5)
           (or (equal current-window-conf-register ?g)
               (equal current-window-conf-register ?r)))
      (progn
        (let ((counter 1)
              (winlen (length windows)))
          (dolist (window windows)
            (if (equal counter (- winlen 2))
                (window-numbering-assign window 8))
            (if (equal counter (- winlen 1))
                (window-numbering-assign window 9))
            (if (equal counter winlen)
                (window-numbering-assign window 0))
            (incf counter))))))

(window-numbering-mode t)
(setq window-numbering-auto-assign-0-to-minibuffer nil)

(provide 'init-window-numbering)
