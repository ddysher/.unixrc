;;------------------------------------------------------------------------------
;; Set shared config vars before loading the rest of the modules.
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

(provide 'init-config-vars)
