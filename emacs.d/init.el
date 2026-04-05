;;------------------------------------------------------------------------------
;; Bool values for init files to decide which functions are available
;;------------------------------------------------------------------------------
;; System type
(defvar *windows* (eq system-type 'windows-nt))
(defvar *cygwin*  (eq system-type 'cygwin))
(defvar *darwin*  (eq system-type 'darwin))
(defvar *linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
;; Emacs major version
(defvar *emacs29* (>= emacs-major-version 29))
(defvar *emacs30* (>= emacs-major-version 30))
;; System name (hostname)
(defvar *home-desktop* (string= system-name "neuralforge"))
(defvar *macpro-m3* (string= system-name "Deyuans-MacBook-M3"))
(defvar *macair-m4* (string= system-name "Deyuans-MacBook-Air"))


;;------------------------------------------------------------------------------
;; Bootstrap configs need to be executed before loading specific configs
;;------------------------------------------------------------------------------
;; This will add "~/.emacs.d/inits" to emacs load path.
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-preload-all-configs)     ; must be called first
(require 'init-elpa-packages)           ; init elpa packages management
(require 'init-site-packages)           ; init third party packages
(require 'init-exec-path-from-shell)    ; init emacs for Mac GUI


;;------------------------------------------------------------------------------
;; Load configs for features and modes
;;------------------------------------------------------------------------------
;; General mode with configurations.
(require 'init-ido)
(require 'init-org)
(require 'init-smex)
(require 'init-gptel)
(require 'init-tramp)
(require 'init-company)
(require 'init-livedown)
(require 'init-yasnippet)
(require 'init-magit-mode)
(require 'init-multi-vterm)
(require 'init-window-numbering)

;; Initialize language related modes that require configurations.
(require 'init-cc-mode)
(require 'init-go-mode)
(require 'init-js2-mode)
(require 'init-web-mode)
(require 'init-rust-mode)
(require 'init-java-mode)
(require 'init-python-mode)
(require 'init-markdown-mode)
(require 'init-more-devtools)

;; My custom mode, functions, etc.
(require 'init-keys)
(require 'init-theme)
(require 'init-custom)
(require 'init-functions)

;; Conditional require per host.
(when *linux*
  (require 'init-pyim))


;;------------------------------------------------------------------------------
;; Automatic generated
;;------------------------------------------------------------------------------
(put 'erase-buffer 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a"
     "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3"
     "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6"
     "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5"
     default))
 '(package-selected-packages
   '(ag apache-mode company dockerfile-mode exec-path-from-shell flycheck
        ggtags go-mode gptel hackernews js2-mode lua-mode magit markdown-mode
        material-theme matlab-mode multi-vterm neotree nginx-mode pet php-mode
        protobuf-mode pyim rust-mode sbt-mode scala-mode smex tern thrift
        vterm web-mode window-numbering wsd-mode yaml-mode yasnippet
        zenburn-theme))
 '(safe-local-variable-values '((c-indent-level . 4)))
 '(send-mail-function 'mailclient-send-it)
 '(wsd-style "roundgreen"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(isearch ((t (:background "green" :foreground "#D0BF8F" :weight bold))))
 '(lazy-highlight ((t (:background "IndianRed4" :foreground "#D0BF8F" :weight bold))))
 '(popup-isearch-match ((t (:background "red" :foreground "#DCDCCC")))))
