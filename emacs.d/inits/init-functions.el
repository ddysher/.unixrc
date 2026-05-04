;;------------------------------------------------------------------------------
;; Custom interactive functions and utilities.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Buffer management
;;------------------------------------------------------------------------------
(defun laura/kill-other-buffers ()
  "Kill all buffers except the current buffer."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun laura/text-scale-increase-all-buffers ()
  "Increase text scale in all buffers by one step."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (text-scale-increase 1)))
  (message "Increased all buffers' size"))

(defun laura/text-scale-decrease-all-buffers ()
  "Decrease text scale in all buffers by one step."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (text-scale-increase -1)))
  (message "Decreased all buffers' size"))

(defun laura/text-scale-reset-all-buffers ()
  "Reset text scale in all buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (text-scale-increase 0)))
  (message "Reset all buffers' size"))

(defun laura/tabspaces-kill-workspace ()
  "Ask for confirmation before killing the current workspace's buffers and closing it."
  (interactive)
  (when (yes-or-no-p "Kill all buffers in this workspace and close it? ")
    (tabspaces-kill-buffers-close-workspace)))

;;------------------------------------------------------------------------------
;; Editing
;;------------------------------------------------------------------------------
(defun laura/scroll-down-in-place (n)
  "Scroll down N lines while moving point by N lines."
  (interactive "p")
  (previous-line n)
  (scroll-down n))

(defun laura/scroll-up-in-place (n)
  "Scroll up N lines while moving point by N lines."
  (interactive "p")
  (next-line n)
  (scroll-up n))

;;------------------------------------------------------------------------------
;; Utilities
;;------------------------------------------------------------------------------
(require 'ansi-color)
(require 'subr-x)

(defgroup laura nil
  "Personal Emacs utilities."
  :group 'convenience)

(defcustom laura/github-workspace-root
  (expand-file-name "~/code/workspace/github.com/")
  "Root directory for GitHub repositories cloned by `laura/open-github-repository'."
  :type 'directory
  :group 'laura)

(defun laura/github-repository-parts (repository)
  "Return (OWNER REPO) parsed from GitHub REPOSITORY.
REPOSITORY may be an HTTPS URL, SSH URL, git@github.com scp-style URL, or
OWNER/REPO shorthand."
  (let* ((trimmed (string-trim repository))
         (without-query (car (split-string trimmed "[?#]")))
         (path
          (cond
           ((string-match "\\`git@github\\.com:\\([^/]+\\)/\\(.+\\)\\'" without-query)
            (concat (match-string 1 without-query) "/" (match-string 2 without-query)))
           ((string-match "\\`ssh://git@github\\.com/\\([^/]+\\)/\\(.+\\)\\'" without-query)
            (concat (match-string 1 without-query) "/" (match-string 2 without-query)))
           ((string-match "\\`https?://github\\.com/\\([^/]+\\)/\\(.+\\)\\'" without-query)
            (concat (match-string 1 without-query) "/" (match-string 2 without-query)))
           (t without-query)))
         (clean-path (string-remove-suffix ".git" (string-remove-suffix "/" path))))
    (unless (string-match "\\`\\([^/]+\\)/\\([^/]+\\)\\'" clean-path)
      (user-error "Expected a GitHub repository like owner/repo or https://github.com/owner/repo"))
    (list (match-string 1 clean-path)
          (match-string 2 clean-path))))

(defun laura/open-github-repository-directory (owner repo repo-dir)
  "Open REPO-DIR and switch to an OWNER/REPO workspace when Tabspaces is loaded."
  (when (fboundp 'tabspaces-switch-or-create-workspace)
    (tabspaces-switch-or-create-workspace (format "%s/%s" owner repo)))
  (find-file repo-dir))

(defun laura/github-clone-filter (process output)
  "Append Git clone OUTPUT from PROCESS to its process buffer."
  (when-let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (let ((moving (= (point) (process-mark process)))
            (inhibit-read-only t))
        (save-excursion
          (goto-char (process-mark process))
          (insert (ansi-color-apply (replace-regexp-in-string "\r" "\n" output)))
          (set-marker (process-mark process) (point)))
        (when moving
          (goto-char (process-mark process)))))))

(defun laura/github-clone-sentinel (process _event)
  "Open the cloned repository when PROCESS exits successfully."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process))
          (owner (process-get process 'laura/github-owner))
          (repo (process-get process 'laura/github-repo))
          (repo-dir (process-get process 'laura/github-repo-dir))
          (repo-url (process-get process 'laura/github-repo-url))
          (exit-code (process-exit-status process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "\nProcess finished with exit code %s\n" exit-code)))))
      (if (zerop exit-code)
          (progn
            (message "Cloned %s to %s" repo-url repo-dir)
            (laura/open-github-repository-directory owner repo repo-dir))
        (when (buffer-live-p buffer)
          (display-buffer buffer))
        (message "git clone failed for %s; see %s"
                 repo-url
                 (if (buffer-live-p buffer) (buffer-name buffer) "clone log buffer"))))))

(defun laura/start-github-clone (owner repo repo-url repo-dir)
  "Clone REPO-URL asynchronously into REPO-DIR for OWNER/REPO."
  (let* ((buffer-name (format "*Git Clone: %s/%s*" owner repo))
         (buffer (get-buffer-create buffer-name))
         (existing-process (get-buffer-process buffer)))
    (if (and existing-process (process-live-p existing-process))
        (progn
          (display-buffer buffer)
          (message "Clone already running for %s/%s" owner repo))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "$ git clone --progress %s %s\n\n"
                          (shell-quote-argument repo-url)
                          (shell-quote-argument repo-dir)))
          (setq buffer-read-only t)))
      (let ((process
             (make-process
              :name buffer-name
              :buffer buffer
              :command (list "git" "clone" "--progress" repo-url repo-dir)
              :connection-type 'pipe
              :filter #'laura/github-clone-filter
              :sentinel #'laura/github-clone-sentinel
              :noquery t)))
        (process-put process 'laura/github-owner owner)
        (process-put process 'laura/github-repo repo)
        (process-put process 'laura/github-repo-url repo-url)
        (process-put process 'laura/github-repo-dir repo-dir)
        (display-buffer buffer)
        (message "Cloning %s into %s..." repo-url repo-dir)))))

(defun laura/open-github-repository (repository)
  "Clone GitHub REPOSITORY into `laura/github-workspace-root' and open it.
Existing repositories are opened without cloning."
  (interactive "sGitHub repository: ")
  (pcase-let* ((`(,owner ,repo) (laura/github-repository-parts repository))
               (repo-url (if (string-match-p "\\`\\(?:https?://\\|ssh://\\|git@\\)" repository)
                             (string-trim repository)
                           (format "https://github.com/%s/%s.git" owner repo)))
               (owner-dir (expand-file-name owner laura/github-workspace-root))
               (repo-dir (expand-file-name repo owner-dir)))
    (cond
     ((file-directory-p repo-dir)
      (message "Opening existing repository: %s" repo-dir)
      (laura/open-github-repository-directory owner repo repo-dir))
     ((file-exists-p repo-dir)
      (user-error "Target exists but is not a directory: %s" repo-dir))
     (t
      (make-directory owner-dir t)
      (laura/start-github-clone owner repo repo-url repo-dir)))))

(defun laura/display-ansi-colors ()
  "Render ANSI color escape sequences in the current buffer.
Useful for log files or pasted shell output containing escape sequences."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun laura/sudo-edit (&optional arg)
  "Edit current file as root.
With prefix ARG, or when the current buffer is not visiting a file, prompt for a
file to open as root."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun laura/show-emacs-pid ()
  "Show the current Emacs process ID."
  (interactive)
  (message "emacs pid: %s" (emacs-pid)))

(provide 'init-functions)
