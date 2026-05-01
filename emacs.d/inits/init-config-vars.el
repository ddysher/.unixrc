;;------------------------------------------------------------------------------
;; Set shared config vars before loading the rest of the modules.
;;------------------------------------------------------------------------------

;; System type
(defconst *darwin-p* (eq system-type 'darwin))
(defconst *linux-p*  (memq system-type '(gnu/linux linux)))

;; System name (hostname)
(defconst *home-desktop* (string= (system-name) "neuralforge"))
(defconst *macpro-m3* (string= (system-name) "Deyuans-MacBook-M3"))
(defconst *macair-m4* (string= (system-name) "Deyuans-MacBook-Air"))

;; Default indent size, used across configurations
(defconst default-indent-size 4)

(provide 'init-config-vars)
