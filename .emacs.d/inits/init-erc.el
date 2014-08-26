;;------------------------------------------------------------------------------
;; Provide erc mode, erc mode is built-in emacs mode.
;;------------------------------------------------------------------------------
(require 'erc)


;; Do not show the following notification.
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
;; Set default erc server to "deyuan.me", which has znc bouncer running.
;; To connect, use "deyuan.me:6667". Note password is username:password.
(setq erc-server "deyuan.me")

(defun erc-mode-custom-hook ()
  (setq truncate-lines t)
  (text-scale-decrease 1))

(add-hook 'erc-mode-hook 'erc-mode-custom-hook)


(provide 'init-erc)
