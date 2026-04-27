;;------------------------------------------------------------------------------
;; agent-tool: spawn a coding agent (claude, codex, ...) in a ghostel buffer
;; rooted at the current project's directory.
;;
;; M-x agent-tool-start prompts for an agent and opens a fresh ghostel
;; session at the project root.  Each invocation spawns a new buffer, so
;; multiple sessions of the same agent can run side by side.
;;------------------------------------------------------------------------------

(require 'project)

(defgroup agent-tool nil
  "Run coding agents in ghostel terminals at the project root."
  :group 'tools)

(defcustom agent-tool-commands
  '((codex . "codex")
    (claude  . "claude")
    (codex-w  . "codex-w")
    (claude-w  . "claude-w")
    (cursor-agent  . "cursor-agent"))
  "Alist of (NAME . PROGRAM) for available coding agents.
NAME is a symbol shown in the prompt; PROGRAM is the executable to run."
  :type '(alist :key-type symbol :value-type string)
  :group 'agent-tool)

(defcustom agent-tool-default nil
  "Default agent symbol pre-selected at the prompt, or nil for no default."
  :type '(choice (const :tag "No default" nil) symbol)
  :group 'agent-tool)

(defun agent-tool--project-root ()
  "Return the project root for `default-directory'.
Try `project-current', then a parent directory containing .git,
then fall back to `default-directory'."
  (or (when-let ((proj (project-current nil)))
        (project-root proj))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun agent-tool--root-name (root)
  "Return a short label for ROOT (its basename)."
  (file-name-nondirectory (directory-file-name (expand-file-name root))))

;;;###autoload
(defun agent-tool-start (agent)
  "Start AGENT in a ghostel terminal rooted at the current project.
Interactively, prompt for the agent from `agent-tool-commands'.
Each call creates a new buffer so multiple sessions can coexist."
  (interactive
   (list
    (let* ((names (mapcar (lambda (c) (symbol-name (car c))) agent-tool-commands))
           (def   (and agent-tool-default (symbol-name agent-tool-default)))
           (pick  (completing-read
                   (format "Agent%s: " (if def (format " (default %s)" def) ""))
                   names nil t nil nil def)))
      (intern pick))))
  (unless (require 'ghostel nil t)
    (error "The ghostel package is required for agent-tool"))
  (let* ((program (or (cdr (assq agent agent-tool-commands))
                      (error "Unknown agent: %s" agent)))
         (root    (agent-tool--project-root))
         (label   (agent-tool--root-name root))
         ;; generate-new-buffer-name appends <2>, <3>, ... for collisions.
         (bufname (generate-new-buffer-name
                   (format "*agent:%s:%s*" agent label)))
         (default-directory (file-name-as-directory root))
         (buffer (get-buffer-create bufname)))
    (ghostel-exec buffer program nil)
    (switch-to-buffer buffer)
    buffer))

(provide 'init-agent-tool)
