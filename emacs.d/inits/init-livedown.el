;;------------------------------------------------------------------------------
;; Provide live markdown mode, installed manually in site-lisp.
;;   https://github.com/shime/emacs-livedown
;; This mode depends on external program, to use this mode, host system must
;; have 'livedown' command installed. To install: run `npm install -g livedown`.
;;------------------------------------------------------------------------------
(use-package livedown
  :ensure nil
  :commands (livedown:preview livedown:kill)
  :custom
  (livedown:autostart nil)
  (livedown:open t)
  (livedown:port 1337))

(provide 'init-livedown)
