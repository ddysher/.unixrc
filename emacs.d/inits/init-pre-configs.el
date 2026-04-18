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

;;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(provide 'init-pre-configs)
