;;------------------------------------------------------------------------------
;; Provide erc mode, erc mode is built-in emacs mode.
;;------------------------------------------------------------------------------
(require 'erc)


;; Do not show the following notification.
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
;; Set default erc server to "deyuan.me", which has znc bouncer running.
;; To connect, use "deyuan.me:6667". Note password is username:password.
;; Original setting is "irc.freenode.net".
(setq erc-server "deyuan.me")

(defun erc-mode-custom-hook ()
  ;; 'erc-ignore-list' is a buffer local variable.
  (setq erc-ignore-list '("*bot" "jenkins-k8s*" "travis*"))
  ;; (text-scale-decrease 1)               ; use smaller size for term
  (setq truncate-lines t) )


(defcustom erc-foolish-content '("*pull request*")
  "Regular expressions to identify foolish content.
Usually what happens is that you add the bots to
`erc-ignore-list' and the bot commands to this list."
  :group 'erc
  :type '(repeat regexp))

;; Not working yet (to disable -[kubot]-
(defun erc-foolish-content (msg)
  "Check whether MSG is foolish."
  (erc-list-match erc-foolish-content msg))

(add-hook
 'erc-insert-pre-hook
 (lambda (s)
   (when (erc-foolish-content s)
     (setq erc-insert-this nil))))

(add-hook 'erc-mode-hook 'erc-mode-custom-hook)


(provide 'init-erc)
