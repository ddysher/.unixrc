;;------------------------------------------------------------------------------
;; Some configs before loading any modules
;;------------------------------------------------------------------------------
;; Set these to nil first, in case error occurs at startup and emacs leaves
;; few junks in current directory.
(setq make-backup-files nil) ;; no backup files (which end with ~)
(setq auto-save-default nil) ;; no autosave files (surrounded by #)

;; Set indent size for nearly all major modes.
(if *goog-desktop*
    ;; Indent width is strictly imposed to 2 in Google code base.
    (setq universal-indent-size 2)
  (setq universal-indent-size 2))

(provide 'init-preload-all-configs)
