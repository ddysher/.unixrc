;;------------------------------------------------------------------------------
;; Provide tramp, tramp mode is built-in with emacs. It works without explicitly
;; enabled. Note we may need to configure tramp-auto-save-directory (or
;; tramp-backup-directory-alist, etc) if auto save is enabled.
;;
;;------------------------------------------------------------------------------
;; Basic usage:
;;   C-x C-f /remotehost:filename  RET (or /method:user@remotehost:filename)
;; E.g:
;;   C-x C-f [F1] -> /deyuan.me:~/Documents/file  RET
;;   C-x C-f [F1] -> /ssh:root@deyuan.me:~/Documents/file  RET
;;------------------------------------------------------------------------------

(use-package tramp
  :ensure nil
  :defer t
  :config
  (setq tramp-default-method "ssh")
  (defun tramp-sh-handle-vc-registered-around (orig-fun &rest args))
  (advice-add 'tramp-sh-handle-vc-registered :around #'tramp-sh-handle-vc-registered-around))

;; Quickly open up a file in remote environment.
(defun tramp-neuralforge-zshrc ()
  (interactive)
  (find-file "/sshx:deyuan@neuralforge:/home/deyuan/.zshrc"))

(defun tramp-devbox-cpu-zshrc ()
  (interactive)
  (find-file "/sshx:dengdeyuan.dengdy@10.37.10.127:/home/dengdeyuan.dengdy/.zshrc"))

(defun tramp-devbox-gpu-l4-zshrc ()
  (interactive)
  (find-file "/sshx:dengdeyuan.dengdy@10.37.96.193:/home/dengdeyuan.dengdy/.zshrc"))

(provide 'init-tramp)
