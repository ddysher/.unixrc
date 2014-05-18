;;------------------------------------------------------------------------------
;; Bool values to decide which functions are available
;;------------------------------------------------------------------------------
;; System type
(defvar *windows*  (eq system-type 'windows-nt))
(defvar *cygwin*   (eq system-type 'cygwin))
(defvar *darwin*   (eq system-type 'darwin))
(defvar *linux*    (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
;; Emacs version
(defvar *emacs23*  (or (>= emacs-major-version 23)))
(defvar *emacs24*  (or (>= emacs-major-version 24)))
;; System name (hostname)
(defvar *home-desktop* (string= system-name "watermelon"))
(defvar *goog-desktop* (string= system-name "deyuan.pit.corp.google.com"))
(defvar *macpro* (string= system-name "deyuan-macbookpro.roam.corp.google.com"))
(defvar *macair* (string= system-name "Deyuans-MacBook-Air.local"))


;;------------------------------------------------------------------------------
;; Bootstrap configs, need to execute before loading specific configs
;;------------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-preload-all-configs) ; preload config, must be called at first
(require 'init-site-packages) ; init third party packages
(require 'init-elpa)          ; init elpa managed packages


;;------------------------------------------------------------------------------
;; Load configs for features and modes
;;------------------------------------------------------------------------------
;; General ones
(require 'init-ido)
(require 'init-erc)
(require 'init-keys)
(require 'init-theme)
(require 'init-tramp)
(require 'init-multi-term)
(require 'init-nginx-mode)
(require 'init-apache-mode)
(require 'init-auto-complete)
(require 'init-window-numbering)
(require 'init-fill-column-indicator)

;; Init language (or related) mode
(require 'init-cc-mode)
(require 'init-go-mode)
(require 'init-lua-mode)
(require 'init-gud-mode)                ; python debug mode
(require 'init-php-mode)
(require 'init-geben-mode)              ; php debug mode
(require 'init-coffee-mode)
(require 'init-python-mode)
(require 'init-markdown-mode)
(require 'init-protobuf-mode)
(require 'init-javascript-mode)
(require 'init-custom)

;; Conditional require
(if *linux*
    (require 'init-w3m))
(if *goog-desktop*
    (load-file "/google/src/head/depot/eng/elisp/google.el"))

;; Temporary & pending mode
(require 'flex-mode)
(require 'cool-mode)
(require 'bison-mode)
;;(require 'init-multi-web)



;;------------------------------------------------------------------------------
;; Automatic generated
;;------------------------------------------------------------------------------
(put 'erase-buffer 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(isearch ((t (:background "green" :foreground "#D0BF8F" :weight bold))))
 '(lazy-highlight ((t (:background "IndianRed4" :foreground "#D0BF8F" :weight bold))))
 '(popup-isearch-match ((t (:background "red" :foreground "#DCDCCC")))))
