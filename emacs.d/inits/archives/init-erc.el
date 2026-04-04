;;------------------------------------------------------------------------------
;; Provide erc mode, erc mode is built-in emacs mode.
;;------------------------------------------------------------------------------
;; Defer erc until first use (M-x erc).
(with-eval-after-load 'erc
  (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (setq erc-server "deyuan.me")
  (setq erc-port 5000)

  (defun erc-mode-custom-hook ()
    (setq erc-ignore-list '("*bot" "jenkins-k8s*" "travis*"))
    (setq truncate-lines t))

  (defcustom erc-foolish-content '("*pull request*")
    "Regular expressions to identify foolish content.
Usually what happens is that you add the bots to
`erc-ignore-list' and the bot commands to this list."
    :group 'erc
    :type '(repeat regexp))

  (defun erc-foolish-content (msg)
    "Check whether MSG is foolish."
    (erc-list-match erc-foolish-content msg))

  (add-hook
   'erc-insert-pre-hook
   (lambda (s)
     (when (erc-foolish-content s)
       (setq erc-insert-this nil))))

  (add-hook 'erc-mode-hook 'erc-mode-custom-hook))

(provide 'init-erc)
