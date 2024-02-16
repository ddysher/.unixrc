;;------------------------------------------------------------------------------
;; Bool values for init files to decide which functions are available
;;------------------------------------------------------------------------------
;; System type

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defvar *windows* (eq system-type 'windows-nt))
(defvar *cygwin*  (eq system-type 'cygwin))
(defvar *darwin*  (eq system-type 'darwin))
(defvar *linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
;; Emacs major version
(defvar *emacs28* (= emacs-major-version 27))
(defvar *emacs29* (= emacs-major-version 29))
;; System name (hostname)
(defvar *home-desktop* (string= system-name "mangosteen"))
(defvar *home-desktop-gpu* (string= system-name "neuralforge"))
(defvar *goog-desktop* (string= system-name "deyuan.pit.corp.google.com"))
(defvar *macpro-13*
  (or (string= system-name "Deyuans-MacBook-Pro.local")
      (string= system-name "Deyuans-MacBook-M1")
      (string= system-name "MacBook-Pro.local")))
(defvar *macpro-16*
  (or (string= system-name "Deyuans-MacBook-Pro-16")
      (string= system-name "MacBook-Pro-16")))
(defvar *macair*
  (or (string= system-name "Deyuans-MacBook-Air.local")
      (string= system-name "MacBook-Air.local")))


;;------------------------------------------------------------------------------
;; Bootstrap configs need to be executed before loading specific configs
;;------------------------------------------------------------------------------
;; This will add "~/.emacs.d/inits" to emacs load path.
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-preload-all-configs)     ; must be called first
(require 'init-site-packages)           ; init third party packages
(require 'init-elpa)                    ; init elpa managed packages
(require 'init-exec-path-from-shell)    ; init emacs for Mac GUI


;;------------------------------------------------------------------------------
;; Load configs for features and modes
;;------------------------------------------------------------------------------
;; General mode with configurations.
(require 'init-w3m)
(require 'init-ido)
(require 'init-erc)
(require 'init-org)
(require 'init-smex)
(require 'init-tramp)
(require 'init-company)
(require 'init-livedown)
(require 'init-yasnippet)
(require 'init-magit-mode)
(require 'init-multi-vterm)
(require 'init-window-numbering)
(require 'init-fill-column-indicator)
;; Disabled general modes, for tracking.
;; (require 'init-helm)
;; (require 'init-multi-term)
;; (require 'init-smartparens-mode)

;; Initialize language related modes that requires configurations.
(require 'init-lsp-mode)
(require 'init-ag-mode)
(require 'init-cc-mode)
(require 'init-sh-mode)
(require 'init-go-mode)
(require 'init-lua-mode)
(require 'init-js2-mode)
(require 'init-web-mode)
(require 'init-rust-mode)
(require 'init-java-mode)
(require 'init-scala-mode)
(require 'init-python-mode)
(require 'init-protobuf-mode)
(require 'init-markdown-mode)
;; Disabled language modes, for tracking.
;; (require 'init-geben-mode)
;; (require 'flex-mode)
;; (require 'cool-mode)
;; (require 'bison-mode))

;; My custom mode, functions, etc.
(require 'init-keys)
(require 'init-theme)
(require 'init-custom)
(require 'init-functions)

;; Simple mode that does not need configurations.
(require 'init-simple-misc-mode)

;; Conditional require: initialize google environment.
(when *goog-desktop*
  (require 'init-google)
  (require 'google))

;; Conditional require: only install pyim in linux (use sougou in darwin).
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
   '("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" default))
 '(livedown:autostart nil)
 '(livedown:open t)
 '(livedown:port 1337)
 '(package-selected-packages
   '(treemacs multi-vterm vterm wsd-mode company lsp-mode pyenv-mode pyim go-projectile projectile ag ein sbt-mode scala-mode protobuf-mode cmake-font-lock cmake-mode neotree matlab-mode wc-mode material-theme edit-server flycheck nginx-mode apache-mode hackernews dockerfile-mode thrift yaml-mode markdown-mode php-mode jade-mode coffee-mode zenburn-theme jedi python-environment epc rust-mode web-mode tern-auto-complete tern js2-mode lua-mode go-mode ggtags fill-column-indicator window-numbering multi-term magit yasnippet helm smex w3m exec-path-from-shell))
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
