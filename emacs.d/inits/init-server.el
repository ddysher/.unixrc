;;------------------------------------------------------------------------------
;; Emacs server for emacsclient.
;;------------------------------------------------------------------------------

(require 'server)

(when (and (fboundp 'server-running-p)
           (not (server-running-p)))
  (server-start))

(provide 'init-server)
