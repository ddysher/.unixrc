;;------------------------------------------------------------------------------
;; Bootstrap configs need to be executed before loading specific configs
;;------------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "inits" user-emacs-directory))
(require 'init-pre-configs)            ; must be called first
(require 'init-elpa-packages)          ; init elpa packages management
(require 'init-exec-path-from-shell)   ; init emacs for Mac GUI


;;------------------------------------------------------------------------------
;; Load configs for features and modes
;;------------------------------------------------------------------------------
;; General mode with configurations.
(require 'init-org)
(require 'init-gptel)
(require 'init-tramp)
(require 'init-completion)
(require 'init-magit-mode)
(require 'init-winum-mode)
(require 'init-multi-vterm)

;; Initialize language related modes that require configurations.
(require 'init-cc-mode)
(require 'init-go-mode)
(require 'init-web-dev)
(require 'init-python-mode)
(require 'init-markdown-mode)
(require 'init-more-devconfigs)

;; My custom mode, functions, etc.
(require 'init-functions)
(require 'init-keys)
(require 'init-theme)
(require 'init-custom)

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
   '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
     "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a"
     "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3"
     "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6"
     "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5"
     default))
 '(package-selected-packages
   '(ag apache-mode cape consult corfu dockerfile-mode doom-modeline
        doom-themes embark embark-consult exec-path-from-shell
        flycheck ggtags go-mode gptel hackernews js2-mode lua-mode
        magit marginalia markdown-mode matlab-mode multi-vterm neotree
        nginx-mode orderless pet php-mode protobuf-mode pyim
        pyim-basedict rust-mode sbt-mode scala-mode tempel thrift
        vertico vterm web-mode winum wsd-mode yaml-mode))
 '(package-vc-selected-packages
   '((livedown :url "https://github.com/shime/emacs-livedown")))
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
