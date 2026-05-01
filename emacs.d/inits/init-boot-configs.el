;;------------------------------------------------------------------------------
;; Set variables and configs before loading any modules.
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

;; Keep custom-file explicit and repo-local so it can be checked in.
;; It is loaded later in init-elpa-packages, after package initialization.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-boot-configs)
