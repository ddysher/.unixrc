;;------------------------------------------------------------------------------
;; Provide window numbering mode, use M-1 ~ M-0 to switch windows
;;------------------------------------------------------------------------------
(require-package 'window-numbering)
(require 'window-numbering)


(defun window-numbering-mode-custom-hook(windows)
  (let ((counter 1))
    (dolist (window windows)
      (if (equal counter 4)
          (window-numbering-assign window 9))
      (if (equal counter 5)
          (window-numbering-assign window 0))
      (incf counter))))

(window-numbering-mode t)


(provide 'init-window-numbering)
