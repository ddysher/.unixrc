;;------------------------------------------------------------------------------
;; Initialize customizations related to google working environment.
;;------------------------------------------------------------------------------
(defun google-g4-google3ify (dir)
  "Find the highest-level parent directory of dir that is named google3."
  (if (null dir) nil
    (let* ((dir (concat dir "/"))
           (first-match (string-match "/google3/" dir)))
      (if first-match
          (substring dir 0 (match-end 0))))))

(defun google-g4-client (dir)
  "Find the name of the current client (based on directory name)."
  (let ((default nil)
        (g3dir (google-g4-google3ify dir)))
    (if (null g3dir) default
      (let* ((dirs (split-string g3dir "/" t))
             (ndirs (length dirs)))
        (if (< ndirs 2) default
          (elt dirs (- ndirs 2)))))))

(defun google3-custom-hook ()
  (let ((client-name (google-g4-client default-directory)))
    ;; If client name is not nil, show client name at mode line.
    (if client-name (add-to-list 'mode-line-format (format "[%s]  " client-name)))))

(defun kill-client-buffers (client-name)
  "Kill all buffers associated with given client."
  (interactive "sEnter client name:")
  (dolist (buffer (buffer-list))
    (if (buffer-file-name buffer)
        (if (string-match client-name (buffer-file-name buffer))
            (kill-buffer buffer)))))

(add-hook 'find-file-hook 'google3-custom-hook)
(add-hook 'dired-file-hook 'google3-custom-hook)
(load-file "/usr/share/emacs/site-lisp/google/google.el")

(provide 'init-google)
