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

(add-hook 'find-file-hook 'google3-custom-hook)
(add-hook 'dired-file-hook 'google3-custom-hook)
(load-file "/google/src/head/depot/eng/elisp/google.el")


(provide 'init-google)
