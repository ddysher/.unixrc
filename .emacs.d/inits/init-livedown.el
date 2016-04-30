;;------------------------------------------------------------------------------
;; Provide live markdown mode, installed manually in site-lisp.
;;   https://github.com/shime/emacs-livedown
;; This mode depends on external program, to use this mode, host system must
;; have 'livedown' command installed. To install: run `npm install -g livedown`.
;;------------------------------------------------------------------------------
(require 'livedown)


(custom-set-variables
 '(livedown:autostart nil) ; automatically open preview when opening markdown files
 '(livedown:open t)        ; automatically open the browser window
 '(livedown:port 1337))    ; port for livedown server

(global-set-key (kbd "C-M-m") 'livedown:preview)


(provide 'init-livedown)
