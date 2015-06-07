;;------------------------------------------------------------------------------
;; Provide live markdown mode, installed manually in site-lisp.
;;------------------------------------------------------------------------------
(require 'livedown)


(custom-set-variables
 '(livedown:autostart nil) ; automatically open preview when opening markdown files
 '(livedown:open t)        ; automatically open the browser window
 '(livedown:port 1337))    ; port for livedown server


(provide 'init-livedown)
