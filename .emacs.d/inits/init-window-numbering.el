;;------------------------------------------------------------------------------
;; Provide window numbering mode, use M-1 ~ M-0 to switch windows
;;------------------------------------------------------------------------------
(require-package 'window-numbering)
(require 'window-numbering)


(defun window-numbering-mode-custom-hook(windows)
  (dolist (window windows)
    (if (equal (buffer-name (window-buffer window)) "*terminal<1>*")
        (window-numbering-assign window 9))
    (if (equal (buffer-name (window-buffer window)) "*terminal<2>*")
        (window-numbering-assign window 0))))

(window-numbering-mode t)


(provide 'init-window-numbering)
