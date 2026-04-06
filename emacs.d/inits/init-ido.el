;;------------------------------------------------------------------------------
;; ido: interactive completion for buffers and files.
;; smex: M-x enhancement built on top of ido; shows recently/frequently used commands.
;;
;; Usage (ido):
;;   C-j  create file even if there is a completion match (also opens directories)
;;   C-l  refresh directory content
;; Usage (smex):
;;   M-x    invoke smex (recently/frequently used commands first)
;;   M-X    smex filtered to major-mode commands only
;;------------------------------------------------------------------------------
(use-package ido
  :ensure nil
  :config
  (ido-mode 'both)
  (setq ido-save-directory-list-file "~/.emacs.d/ido.last"
        ido-ignore-buffers
        '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
          "^\*compilation" "^\*TAGS" "^session\.*" "^\*scratch"
          "^\*Help" "^\*tramp" "^\*Compile-Log")
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

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))

(provide 'init-ido)
