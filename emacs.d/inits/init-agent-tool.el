;;; init-agent-tool.el --- Run coding agents in ghostel  -*- lexical-binding: t; -*-
;;------------------------------------------------------------------------------
;; agent-tool: spawn a coding agent (claude, codex, ...) in a ghostel buffer.
;;
;; M-x agent-tool-start prompts for an agent and opens a fresh ghostel
;; session at the project root.  Directory override and resume/continue
;; flow live in the transient menu (added in a later phase).
;;
;; Each agent is a plist in `agent-tool-agents' so resume/continue flags
;; and extra args can be declared per agent without touching call sites.
;; Buffer name is left to ghostel (it rewrites it via OSC 2 anyway);
;; identity lives in the buffer-local `agent-tool--session' plist.
;;------------------------------------------------------------------------------

(require 'cl-lib)
(require 'project)

(defvar ghostel-buffer-name)
(declare-function ghostel-exec "ghostel")

(defgroup agent-tool nil
  "Run coding agents in ghostel terminals."
  :group 'tools)

(defcustom agent-tool-agents
  '((codex        :program "codex"        :resume-flag "--resume" :continue-flag "--continue")
    (claude       :program "claude"       :resume-flag "--resume" :continue-flag "--continue")
    (codex-w      :program "codex-w"      :resume-flag "--resume" :continue-flag "--continue")
    (claude-w     :program "claude-w"     :resume-flag "--resume" :continue-flag "--continue")
    (cursor-agent :program "cursor-agent"))
  "Alist of (NAME . PLIST) for available coding agents.

NAME is a symbol shown in the prompt.  PLIST keys:
  :program        Executable name or path (string, required).
  :resume-flag    Flag that opens the tool's native session picker, or nil.
  :continue-flag  Flag that resumes the last session, or nil.
  :extra-args     List of additional argv strings appended to every launch."
  :type '(alist :key-type symbol
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'agent-tool)

(defcustom agent-tool-default nil
  "Default agent symbol pre-selected at the prompt, or nil for no default."
  :type '(choice (const :tag "No default" nil) symbol)
  :group 'agent-tool)

(defcustom agent-tool-confirm-kill t
  "When non-nil, prompt before killing a buffer with a live agent process."
  :type 'boolean
  :group 'agent-tool)

(defcustom agent-tool-sidebar-width 36
  "Width in columns of the agent-tool sidebar window."
  :type 'integer
  :group 'agent-tool)

(defcustom agent-tool-sidebar-buffer-name "*agent-tool sidebar*"
  "Buffer name used by the agent-tool sidebar."
  :type 'string
  :group 'agent-tool)

(defvar-local agent-tool--session nil
  "Buffer-local plist describing this buffer's agent session.
Keys: :agent :dir :resume-mode :started-at.  Set on launch and never
overwritten — ghostel may rename the buffer via OSC 2, but this plist
remains the source of truth for session identity.")

(defvar agent-tool--sessions nil
  "List of live buffers spawned by `agent-tool-start' & friends.
Entries are pruned by `agent-tool--forget-buffer' on `kill-buffer-hook'.")

(defun agent-tool--project-root ()
  "Return the project root for `default-directory'.
Try `project-current', then a parent directory containing .git,
then fall back to `default-directory'."
  (or (when-let ((proj (project-current nil)))
        (project-root proj))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun agent-tool--agent-plist (agent)
  "Return the plist for AGENT or signal an error if unknown."
  (or (cdr (assq agent agent-tool-agents))
      (error "Unknown agent: %s" agent)))

(defun agent-tool--read-agent ()
  "Prompt for an agent symbol from `agent-tool-agents'."
  (let* ((names (mapcar (lambda (c) (symbol-name (car c))) agent-tool-agents))
         (def   (and agent-tool-default (symbol-name agent-tool-default)))
         (pick  (completing-read
                 (format "Agent%s: " (if def (format " (default %s)" def) ""))
                 names nil t nil nil def)))
    (intern pick)))

(defun agent-tool--forget-buffer ()
  "Remove the current buffer from `agent-tool--sessions'."
  (setq agent-tool--sessions (delq (current-buffer) agent-tool--sessions)))

(defun agent-tool--confirm-kill ()
  "Confirm killing the current buffer when it hosts a live agent.
Hooked into `kill-buffer-query-functions'.  Returns non-nil to allow
the kill, nil to abort.  Skipped when `agent-tool-confirm-kill' is nil,
when the buffer is not an agent session, or when the agent process has
already exited."
  (if (and agent-tool-confirm-kill
           agent-tool--session
           (let ((proc (get-buffer-process (current-buffer))))
             (and proc (process-live-p proc))))
      (let* ((agent (plist-get agent-tool--session :agent))
             (dir   (plist-get agent-tool--session :dir)))
        (y-or-n-p (format "Kill running %s session in %s? " agent dir)))
    t))

(add-hook 'kill-buffer-query-functions #'agent-tool--confirm-kill)

(defun agent-tool--launch (agent dir &optional resume-mode)
  "Launch AGENT in DIR and return the ghostel buffer.
RESUME-MODE is nil, `pick' (use :resume-flag), or `continue'
(use :continue-flag).  Errors if the requested flag is unset for AGENT."
  (unless (require 'ghostel nil t)
    (error "The ghostel package is required for agent-tool"))
  (let* ((plist   (agent-tool--agent-plist agent))
         (program (or (plist-get plist :program)
                      (error "Agent %s has no :program" agent)))
         (extra   (plist-get plist :extra-args))
         (flag    (pcase resume-mode
                    ('nil      nil)
                    ('pick     (or (plist-get plist :resume-flag)
                                   (error "Agent %s has no :resume-flag" agent)))
                    ('continue (or (plist-get plist :continue-flag)
                                   (error "Agent %s has no :continue-flag" agent)))
                    (_ (error "Bad resume-mode: %s" resume-mode))))
         (args    (delq nil (append (and flag (list flag)) extra)))
         (default-directory (file-name-as-directory dir))
         (buffer  (generate-new-buffer ghostel-buffer-name)))
    (switch-to-buffer buffer)
    (push buffer agent-tool--sessions)
    ;; ghostel-exec switches the buffer into ghostel-mode, which calls
    ;; kill-all-local-variables — so set our buffer-local plist *after*.
    (ghostel-exec buffer program args)
    (with-current-buffer buffer
      (setq agent-tool--session
            (list :agent       agent
                  :dir         default-directory
                  :resume-mode resume-mode
                  :started-at  (current-time)))
      (add-hook 'kill-buffer-hook #'agent-tool--forget-buffer nil t))
    buffer))

;;------------------------------------------------------------------------------
;; Sidebar — dirvish-style left side window listing live agent sessions.
;;------------------------------------------------------------------------------

(defun agent-tool--session-live-p (buffer)
  "Return non-nil when BUFFER hosts a live agent process."
  (let ((proc (get-buffer-process buffer)))
    (and proc (process-live-p proc))))

(defun agent-tool--sidebar-row (buffer)
  "Return a tabulated-list row for session BUFFER."
  (let* ((session (buffer-local-value 'agent-tool--session buffer))
         (agent   (plist-get session :agent))
         (dir     (plist-get session :dir))
         (live    (agent-tool--session-live-p buffer))
         (status  (if live "●" "○")))
    (list buffer
          (vector status
                  (if agent (symbol-name agent) "?")
                  (abbreviate-file-name (or dir ""))
                  (buffer-name buffer)))))

(defun agent-tool--sidebar-entries ()
  "Return tabulated-list entries for live sidebar rows.
Prunes dead buffers from `agent-tool--sessions' as a side effect."
  (setq agent-tool--sessions
        (cl-remove-if-not #'buffer-live-p agent-tool--sessions))
  (mapcar #'agent-tool--sidebar-row agent-tool--sessions))

(defun agent-tool--sidebar-buffer-at-point ()
  "Return the session buffer for the row at point, or signal."
  (or (tabulated-list-get-id)
      (user-error "No agent session on this line")))

(defun agent-tool-sidebar-visit ()
  "Switch to the agent buffer on this row."
  (interactive)
  (let ((buf (agent-tool--sidebar-buffer-at-point)))
    (pop-to-buffer buf '((display-buffer-reuse-window
                          display-buffer-use-some-window)
                         (inhibit-same-window . nil)))))

(defun agent-tool-sidebar-visit-other-window ()
  "Display the agent buffer on this row in another window, keep focus here."
  (interactive)
  (let ((buf (agent-tool--sidebar-buffer-at-point)))
    (display-buffer buf '((display-buffer-use-some-window
                           display-buffer-pop-up-window)))))

(defun agent-tool-sidebar-kill ()
  "Kill the agent buffer on this row (using the standard confirm path)."
  (interactive)
  (let ((buf (agent-tool--sidebar-buffer-at-point)))
    (when (kill-buffer buf)
      (tabulated-list-revert))))

(defvar agent-tool-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-tool-sidebar-visit)
    (define-key map (kbd "o")   #'agent-tool-sidebar-visit-other-window)
    (define-key map (kbd "k")   #'agent-tool-sidebar-kill)
    (define-key map (kbd "g")   #'tabulated-list-revert)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `agent-tool-sidebar-mode'.")

(define-derived-mode agent-tool-sidebar-mode tabulated-list-mode "Agent-Sidebar"
  "Major mode for the agent-tool sidebar."
  (setq tabulated-list-format
        [(""        2 nil)
         ("Agent"  14 t)
         ("Dir"    24 t)
         ("Buffer" 30 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-entries #'agent-tool--sidebar-entries)
  (tabulated-list-init-header))

(defun agent-tool--sidebar-refresh-if-visible ()
  "Refresh the sidebar buffer if it exists and is currently displayed."
  (when-let* ((buf (get-buffer agent-tool-sidebar-buffer-name))
              ((get-buffer-window buf t)))
    (with-current-buffer buf
      (let ((inhibit-message t))
        (tabulated-list-revert)))))

(add-hook 'kill-buffer-hook #'agent-tool--sidebar-refresh-if-visible)

;;;###autoload
(defun agent-tool-sidebar ()
  "Toggle the agent-tool sidebar in a left side window."
  (interactive)
  (let* ((buf-name agent-tool-sidebar-buffer-name)
         (existing (get-buffer-window (get-buffer buf-name) t)))
    (if existing
        (delete-window existing)
      (let ((buf (get-buffer-create buf-name)))
        (with-current-buffer buf
          (unless (derived-mode-p 'agent-tool-sidebar-mode)
            (agent-tool-sidebar-mode))
          (tabulated-list-revert))
        (display-buffer-in-side-window
         buf `((side . left)
               (slot . -1)
               (window-width . ,agent-tool-sidebar-width)
               (window-parameters . ((no-other-window . nil)
                                     (no-delete-other-windows . t)))))
        (select-window (get-buffer-window buf t))))))

;;;###autoload
(defun agent-tool-start (agent)
  "Start AGENT in a ghostel terminal at the current project's root.
Interactively, prompt for the agent from `agent-tool-agents'."
  (interactive (list (agent-tool--read-agent)))
  (agent-tool--launch agent (agent-tool--project-root) nil))

(provide 'init-agent-tool)
