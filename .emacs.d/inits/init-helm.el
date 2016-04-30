;;------------------------------------------------------------------------------
;; Helm is incremental completion and selection narrowing framework for Emacs.
;; https://github.com/emacs-helm/helm
;;------------------------------------------------------------------------------
(require-package 'helm)
(require 'helm)


;; ;; Basic config from helm, copied from elpa/helm-xxxx/helm-config.el. Main
;; ;; customization is to disable helm find files and use ido. Helm grep, etc
;; ;; is enabled.
;; (require 'helm-config)

(helm-mode 1)
(helm-adaptive-mode 1)


(defun helm-do-grep-recursive ()
  "Like `helm-do-grep', but greps recursively by default and targeting current directory."
  ;; Still using interactive, as we need to be able to ask for the pattern and
  ;; files type
  (interactive)
    (helm-do-grep-1 (list default-directory) t nil))


(provide 'init-helm)
