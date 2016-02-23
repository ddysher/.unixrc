;;------------------------------------------------------------------------------
;; Helm is incremental completion and selection narrowing framework for Emacs.
;; https://github.com/emacs-helm/helm
;;------------------------------------------------------------------------------
(require-package 'helm)
(require 'helm)


;; Basic config from helm, copied from elpa/helm-xxxx/helm-config.el. Main
;; customization is to disable helm find files and use ido. Helm grep, etc
;; is enabled.
(require 'helm-config)

(helm-mode 1)
(helm-adaptive-mode 1)


(provide 'init-helm)
