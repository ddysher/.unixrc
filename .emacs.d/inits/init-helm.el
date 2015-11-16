;;------------------------------------------------------------------------------
;; Helm is incremental completion and selection narrowing framework for Emacs.
;; https://github.com/emacs-helm/helm
;;------------------------------------------------------------------------------
(require-package 'helm)
(require 'helm)


;; Basic config from helm, see elpa/helm-xxxx/helm-config.el
(require 'helm-config)

(helm-mode 1)
(helm-adaptive-mode 1)
(setq helm-ff-auto-update-initial-value t)


(provide 'init-helm)
