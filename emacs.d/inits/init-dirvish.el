;;------------------------------------------------------------------------------
;; Dirvish - modern file navigation on top of Dired.
;;------------------------------------------------------------------------------

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-attributes '(nerd-icons file-size file-time collapse subtree-state vc-state))
  (dirvish-side-width 36)
  (dirvish-hide-details t)
  (dirvish-quick-access-entries
   `(("h" "~/" "Home")
     ("u" ,(expand-file-name "~/.unixrc/") "Unixrc")
     ("e" ,user-emacs-directory "Emacs")
     ("d" "~/Downloads/" "Downloads")
     ("t" "/tmp/" "Temp")))
  :bind
  (:map dirvish-mode-map
   ("?" . dirvish-dispatch)
   ("a" . dirvish-quick-access)
   ("h" . dirvish-history-jump)
   ("/" . dirvish-narrow)
   ("TAB" . dirvish-subtree-toggle)))

(provide 'init-dirvish)
