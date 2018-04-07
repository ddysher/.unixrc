;;------------------------------------------------------------------------------
;; Provide tramp, tramp mode is built-in with emacs. It works even not required
;; explicitly.  Note we may need to configure tramp-auto-save-directory (or
;; tramp-backup-directory-alist, etc) if auto save is enabled.
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

;; Quickly open up a file in vagrant dev environment.
;; Usage:
;;   M-x tramp-vagrant-kubernetes
;; Requirements:
;;   1. ubuntu box is running at IP address 192.168.33.33
;;   2. kubernetes located in /home/vagrant/code/workspace/src/k8s.io/kubernetes
;;   3. can ssh to ubuntu box without password
(defun tramp-vagrant-kubernetes ()
  (interactive)
  (find-file "/sshx:vagrant@192.168.33.33:/home/vagrant/code/workspace/src/k8s.io/kubernetes/README.md"))

(defun tramp-mangosteen-org ()
  (interactive)
  (find-file "/sshx:deyuan@192.168.3.34:/home/deyuan/code/general.org"))

(defun tramp-sugarcane-org ()
  (interactive)
  (find-file "/sshx:deyuan@192.168.8.53:/home/deyuan/code/general.org"))

(provide 'init-tramp)
