;;------------------------------------------------------------------------------
;; Provide erc mode, erc mode is built-in emacs mode.
;;------------------------------------------------------------------------------
;; Ignore these notice.
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(defun erc-mode-custom-hook ()
  (setq truncate-lines t)
  (text-scale-decrease 1))

(add-hook 'erc-mode-hook 'erc-mode-custom-hook)


(provide 'init-erc)
