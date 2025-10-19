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
(require 'tramp)

(setq tramp-default-method "ssh")

;; Define a function to advice around 'tramp-sh-handle-vc-registered'. The
;; function doesn't call orign-func, which means 'tramp-sh-handle-vc-registered'
;; is disabled. 'tramp-sh-handle-vc-registered-around' is defined in tramp.el
;; to manage files opened under version control system (e.g. if the file in
;; remote host is controlled via git, then the buffer will show Git-master).
;; The process is slow and log is spammy, and using vc in tramp is not common,
;; so disable it.
(defun tramp-sh-handle-vc-registered-around (orig-fun &rest args))
(advice-add 'tramp-sh-handle-vc-registered :around #'tramp-sh-handle-vc-registered-around)

;; Quickly open up a file in remote environment.
;;
;; Example Usage:
;;   M-x tramp-mangosteen
;;
;; Requirements:
;;   - file '.zshrc' exists
;;   - password-less ssh
(defun tramp-mangosteen-zshrc ()
  (interactive)
  (find-file "/sshx:deyuan@mangosteen:/home/deyuan/.zshrc"))

(defun tramp-sugarcane-zshrc ()
  (interactive)
  (find-file "/sshx:deyuan@sugarcane:/home/deyuan/.zshrc"))

(defun tramp-cherries-zshrc ()
  (interactive)
  (find-file "/sshx:deyuan@cherries:/home/deyuan/.zshrc"))

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
