;;------------------------------------------------------------------------------
;; Some configs before loading any modules
;;------------------------------------------------------------------------------
(if *goog-desktop*
    (setq universal-indent-size 2)
  (setq universal-indent-size 4))

;; Set these first, in case error occurs at startup.
(setq make-backup-files nil) ;; no backup files (which end with ~)
(setq auto-save-default nil) ;; no autosave files (surrounded by #)


(provide 'init-preload-all-configs)
