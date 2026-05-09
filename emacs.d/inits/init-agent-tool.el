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

;;;###autoload
(defun agent-tool-start (agent)
  "Start AGENT in a ghostel terminal at the current project's root.
Interactively, prompt for the agent from `agent-tool-agents'."
  (interactive (list (agent-tool--read-agent)))
  (agent-tool--launch agent (agent-tool--project-root) nil))

(provide 'init-agent-tool)
