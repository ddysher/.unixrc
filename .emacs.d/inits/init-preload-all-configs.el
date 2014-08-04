;;------------------------------------------------------------------------------
;; Some configs before loading any modules
;;------------------------------------------------------------------------------
;; Set indent size for nearly all major modes.
(if *goog-desktop*
    ;; Strictly imposed to 2 in Google, personal preference subject to change.
    (setq universal-indent-size 2)
  (setq universal-indent-size 2))

;; Set these first, in case error occurs at startup.
(setq make-backup-files nil) ;; no backup files (which end with ~)
(setq auto-save-default nil) ;; no autosave files (surrounded by #)


(provide 'init-preload-all-configs)
