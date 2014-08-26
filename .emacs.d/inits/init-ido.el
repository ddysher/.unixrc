;;------------------------------------------------------------------------------
;; Provide ido, which makes completing buffers and finding files much easier.
;; Note ido creates "ido.last" file under ~/.emacs.d directory.
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
;; Usage:
;;   C-j create file even if there is completion for the name. e.g. create file
;;       init.el when there is init-abc.el.
;;   C-l refresh directory content.
;;------------------------------------------------------------------------------
(require 'ido)


(ido-mode 'both)                        ; for buffers and files
(setq ido-save-directory-list-file "~/.emacs.d/ido.last"
      ido-ignore-buffers                ; ignore these guys
      '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
        "^\*epc" "^\*compilation" "^\*TAGS" "^session\.*" "^\*scratch"
        "^\*Help" "^\*tramp" "^\*Compile-Log" "^\*go-eldoc")
      ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
      ido-enable-last-directory-history t ; remember last used dirs
      ido-max-work-directory-list 30      ; should be enough
      ido-max-work-file-list      50      ; remember many
      ido-use-filename-at-point nil ; don't use filename at point (annoying)
      ido-use-url-at-point nil     ; don't use url at point (annoying)
      ido-enable-flex-matching nil ; don't try to be too smart
      ido-max-prospects 20         ; don't spam my minibuffer
      ido-auto-merge-work-directories-length -1 ; do not auto merge
      ido-confirm-unique-completion t) ; wait for RET, even with unique completion


(defun sudo-edit (&optional arg)
  ;; Edit currently visited file as root, or open a new file as root if current
  ;; buffer does not associate with a file.
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(provide 'init-ido)
