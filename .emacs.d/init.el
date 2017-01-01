;;------------------------------------------------------------------------------
;; Bool values for init files to decide which functions are available
;;------------------------------------------------------------------------------
;; System type
(defvar *windows* (eq system-type 'windows-nt))
(defvar *cygwin*  (eq system-type 'cygwin))
(defvar *darwin*  (eq system-type 'darwin))
(defvar *linux*   (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
;; Emacs version
(defvar *emacs23* (= emacs-major-version 23))
(defvar *emacs24* (= emacs-major-version 24))
(defvar *emacs25* (= emacs-major-version 25))
;; System name (hostname)
(defvar *home-desktop* (string= system-name "watermelon"))
(defvar *goog-desktop* (string= system-name "deyuan.pit.corp.google.com"))
(defvar *macpro*
  (or (string= system-name "Deyuans-MacBook-Pro.local")
      (string= system-name "MacBook-Pro.local")))
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
(require 'init-smex)
(require 'init-helm)
(require 'init-tramp)
(require 'init-org-mode)
(require 'init-livedown)
(require 'init-yasnippet)
(require 'init-magit-mode)
(require 'init-multi-term)
(require 'init-auto-complete)
(require 'init-window-numbering)
(require 'init-fill-column-indicator)
;; Disabled general modes, for tracking.
;; (require 'init-smartparens-mode)

;; Initialize language related modes that requires configurations.
(require 'init-cc-mode)
(require 'init-sh-mode)
(require 'init-go-mode)
(require 'init-lua-mode)
(require 'init-js2-mode)
(require 'init-web-mode)
(require 'init-gud-mode)
(require 'init-rust-mode)
(require 'init-java-mode)
(require 'init-python-mode)
;; Disabled language modes, for tracking
;; (require 'init-geben-mode)
;; (require 'init-protobuf-mode)
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

;; Conditional require
(when *goog-desktop*
  (require 'init-google)
  (require 'google))

;; Only install chinese-pyim in linux: we can use sougou in Mac easily.
(when *linux*
  (require 'init-chinese-pyim))


;;------------------------------------------------------------------------------
;; Automatic generated
;;------------------------------------------------------------------------------
(put 'erase-buffer 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(livedown:autostart nil)
 '(livedown:open t)
 '(livedown:port 1337)
 '(safe-local-variable-values (quote ((c-indent-level . 4))))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(isearch ((t (:background "green" :foreground "#D0BF8F" :weight bold))))
 '(lazy-highlight ((t (:background "IndianRed4" :foreground "#D0BF8F" :weight bold))))
 '(popup-isearch-match ((t (:background "red" :foreground "#DCDCCC")))))
