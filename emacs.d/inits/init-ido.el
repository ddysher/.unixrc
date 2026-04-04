;;------------------------------------------------------------------------------
;; Provide ido, which makes completing buffers and finding files much easier.
;; Note ido creates "ido.last" file under "~/.emacs.d" directory.
;;   http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
;;
;;------------------------------------------------------------------------------
;; Usage:
;;   C-j create file even if there is completion for the name. e.g. create file
;;       init.el when there is init-abc.el. Also used to open directory.
;;   C-l refresh directory content.
;;------------------------------------------------------------------------------
(use-package ido
  :ensure nil
  :config
  (ido-mode 'both)
  (setq ido-save-directory-list-file "~/.emacs.d/ido.last"
        ido-ignore-buffers
        '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
          "^\*epc" "^\*compilation" "^\*TAGS" "^session\.*" "^\*scratch"
          "^\*Help" "^\*tramp" "^\*Compile-Log" "^\*go-eldoc")
        ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
        ido-enable-last-directory-history t
        ido-max-work-directory-list 30
        ido-max-work-file-list      50
        ido-use-filename-at-point nil
        ido-use-url-at-point nil
        ido-enable-flex-matching nil
        ido-max-prospects 20
        ido-auto-merge-work-directories-length -1
        ido-confirm-unique-completion t)
  (add-to-list 'ido-ignore-files ".DS_Store")
  (add-to-list 'ido-ignore-files ".owncloudsync.log")
  (add-to-list 'ido-ignore-files "._sync")
  (add-to-list 'ido-ignore-files ".csync_journal.db")
  (add-to-list 'ido-ignore-files ".csync_journal.db-shm")
  (add-to-list 'ido-ignore-files ".csync_journal.db-wal"))

(provide 'init-ido)
