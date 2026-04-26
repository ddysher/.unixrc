;;------------------------------------------------------------------------------
;; Configs before loading any modules
;;------------------------------------------------------------------------------

;; System type
(defvar *darwin*  (eq system-type 'darwin))
(defvar *linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))

;; System name (hostname)
(defvar *home-desktop* (string= system-name "neuralforge"))
(defvar *macpro-m3* (string= system-name "Deyuans-MacBook-M3"))
(defvar *macair-m4* (string= system-name "Deyuans-MacBook-Air"))

;; Default indent size, used across configurations
(defvar universal-indent-size 4)

;; Set auto-managed directory
(setq savehist-file               (expand-file-name "history"        auto-managed-dir)
      project-list-file           (expand-file-name "projects"       auto-managed-dir)
      recentf-save-file           (expand-file-name "recentf"        auto-managed-dir)
      tramp-persistency-file-name (expand-file-name "tramp"          auto-managed-dir)
      srecode-map-save-file       (expand-file-name "srecode-map.el" auto-managed-dir)
      auto-save-list-file-prefix  (expand-file-name "auto-save-list/.saves-" auto-managed-dir)
      url-configuration-directory (expand-file-name "url/"           auto-managed-dir)
      transient-history-file      (expand-file-name "transient/history.el" auto-managed-dir)
      transient-values-file       (expand-file-name "transient/values.el"  auto-managed-dir)
      transient-levels-file       (expand-file-name "transient/levels.el"  auto-managed-dir))

;; Set User-data directory. user-data-dir is set in early-init.el
;; - templates file path are set in tempel package.
(setq bookmark-default-file (expand-file-name "bookmarks" user-data-dir)
      ;; Custom file path. Loaded later in init-elpa-packages, after
      ;; (package-initialize), so that 'package-selected-packages' is a bound
      ;; defcustom by the time custom-set-variables applies its saved value.
      custom-file           (expand-file-name "custom.el" user-data-dir))

(provide 'init-boot-configs)
