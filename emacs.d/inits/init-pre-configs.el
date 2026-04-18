;;------------------------------------------------------------------------------
;; Configs before loading any modules
;;------------------------------------------------------------------------------

;;; System type
(defvar *windows* (eq system-type 'windows-nt))
(defvar *cygwin*  (eq system-type 'cygwin))
(defvar *darwin*  (eq system-type 'darwin))
(defvar *linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
;;; Emacs major version
(defvar *emacs29* (>= emacs-major-version 29))
(defvar *emacs30* (>= emacs-major-version 30))
;;; System name (hostname)
(defvar *home-desktop* (string= system-name "neuralforge"))
(defvar *macpro-m3* (string= system-name "Deyuans-MacBook-M3"))
(defvar *macair-m4* (string= system-name "Deyuans-MacBook-Air"))

;;; Default indent size, used across configurations
(defvar universal-indent-size 4)

;;; Auto-managed directory for runtime-generated files
(defvar auto-managed-dir (expand-file-name "auto-managed" user-emacs-directory))
(make-directory auto-managed-dir t)

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

;;; User-data directory for hand-managed data files
(defvar user-data-dir (expand-file-name "user-data" user-emacs-directory))
(setq bookmark-default-file (expand-file-name "bookmarks" user-data-dir))

;;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(provide 'init-pre-configs)
