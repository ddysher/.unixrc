;;------------------------------------------------------------------------------
;; Python mode configuration: Eglot + Pyright + python-ts-mode + uv
;;------------------------------------------------------------------------------
;; Uses eglot (built-in Emacs 29+) with pyright as the language server, and
;; python-ts-mode (tree-sitter) for superior syntax highlighting/indentation.
;; Eglot integrates with standard Emacs infrastructure: xref for navigation,
;; flymake for diagnostics, eldoc for docs, capf for completions (via company).
;;
;; Navigation, bound by eglot via xref.
;;   M-. xref-find-definitions
;;   M-, xref-go-back
;;
;; Virtual environment detection (pet + uv):
;;   emacs-pet automatically detects project venvs (.venv, pyproject.toml, etc.)
;;   and configures python-shell-interpreter, eglot/pyright, and flycheck.
;;   For projects without a venv, a default venv at ~/.local/share/uv/default-venv
;;   is used as a fallback (Python 3.13).
;;
;;   To create a project venv:  uv venv && uv sync
;;   Default venv packages:     uv pip install --python ~/.local/share/uv/default-venv/bin/python <pkg>
;;
;; Dependencies:
;;   uv - Python package manager: https://docs.astral.sh/uv/
;;   pyright - language server: npm install -g pyright (or: uv pip install pyright)
;;   python tree-sitter grammar - install once via:
;;     M-x treesit-install-language-grammar -> python
;;
;;------------------------------------------------------------------------------
;; Debugging (GUD / pdb)
;;
;; Usage:
;;   M-x pdb, then input the script path
;;   F9   gud-cont   F10  gud-next
;;   F11  gud-step   F12  gud-break
;;
;;------------------------------------------------------------------------------

;; Default venv for projects without their own .venv.
(defvar uv-default-venv (expand-file-name "~/.local/share/uv/default-venv"))

;; Remap python-mode to python-ts-mode for tree-sitter powered highlighting.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; Eglot is built-in (Emacs 29+). Pyright is auto-detected if installed;
;; explicitly listed here to prefer it over the default pylsp.
;;
;; eglot-workspace-configuration is set as a global function (not buffer-local,
;; since eglot reads it from a temp buffer where buffer-local values are lost).
;; When pyright requests workspace config, eglot sets default-directory to the
;; file's directory, so pet-virtualenv-root resolves correctly per-project.
;; If no project venv is found, we fall back to uv-default-venv.
(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (setq-default eglot-workspace-configuration
                (lambda (_server)
                  (when-let* ((venv (or (pet-virtualenv-root)
                                        (and (file-directory-p uv-default-venv)
                                             uv-default-venv)))
                              (python (expand-file-name "bin/python" venv)))
                    (list :python
                          (list :pythonPath python
                                :venvPath (file-name-directory
                                           (directory-file-name venv))
                                :venv (file-name-nondirectory
                                       (directory-file-name venv))))))))

;; pet: detect project virtualenvs for python-shell-interpreter / run-python.
;; We do NOT use pet-eglot-setup — its advice overrides our fallback with nil
;; values when no project venv exists. Eglot config is handled above instead.
(use-package pet
  :ensure t
  :config
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (when (and (not python-shell-virtualenv-root)
                         (file-directory-p uv-default-venv))
                (setq-local python-shell-virtualenv-root uv-default-venv
                            python-shell-interpreter
                            (expand-file-name "bin/python" uv-default-venv)))
              (eglot-ensure))))

(defun python-mode-custom-hook ()
  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key (kbd "C-c d") 'eldoc-doc-buffer)
  (setq python-indent-offset universal-indent-size))

(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-ts-mode))
(add-hook 'python-ts-mode-hook 'python-mode-custom-hook)

(defun gud-mode-custom-hook ()
  (define-key gud-mode-map '[f9] 'gud-cont)
  (define-key gud-mode-map '[f10] 'gud-next)
  (define-key gud-mode-map '[f11] 'gud-step)
  (define-key gud-mode-map '[f12] 'gud-break))

(add-hook 'gud-mode-hook 'gud-mode-custom-hook)

(provide 'init-python-mode)

;;------------------------------------------------------------------------------
;; Historical: V1 Autocompletion via Jedi + EPC
;;------------------------------------------------------------------------------
;; Autocompletion in python mode uses emacs-jedi, which in turn uses quite a few
;; other libraries. It helps you to find information about Python objects, such
;; as docstring, function arguments and code location, see http://tkf.github.io/emacs-jedi
;;
;; To install emacs-jedi, we need following packages in emacs:
;;   epc - a rpc stack for emacs: https://github.com/kiwanami/emacs-epc
;;   deferred - asynchronous functions for emacs, this is used internally by epc: https://github.com/kiwanami/emacs-deferred
;;   auto-complete - emacs completion framework: https://github.com/auto-complete/auto-complete
;;   python-environment - virtualenv API for emacs: https://github.com/tkf/emacs-python-environment
;;
;; Following command and package need to be installed in host os (use pip install):
;;   virtualenv - create isolated python environments for emacs (located at ~/.emacs.d/.python-environments)
;;   jedi - autocompletion and static analysis library for python, https://github.com/davidhalter/jedi
;;   python-epc (pip install epc) - python implementation of emacs epc stack, https://github.com/tkf/python-epc
;;   argparse - parsing args
;;
;; Install Python server (jediepcserver.py) by running:
;;   M-x jedi:install-server in Emacs
;; The command only needs to run once.
;;
;; How Jedi.el works (http://tkf.github.io/emacs-jedi/latest/#how-it-works):
;; Jedi.el calls Python methods in jedi through EPC protocol. Emacs side
;; implementation of EPC is epc.el, and Python side is python-epc. A python
;; process will be running at host.
;;
;;------------------------------------------------------------------------------
;; Historical: V1 Jedi navigation functions
;;------------------------------------------------------------------------------
;; These implemented a manual jump stack on top of jedi:goto-definition so that
;; M-, would pop back through a series of definition jumps. Eglot delegates this
;; to xref (xref-go-back), which maintains the same stack automatically.
;;
;; (defun jedi:jump-to-definition ()
;;   (interactive)
;;   (require 'jedi)
;;   (add-to-list 'jedi:goto-stack
;;                (list (buffer-name) (point)))
;;   (jedi:goto-definition))
;;
;; (defun jedi:jump-back ()
;;   (interactive)
;;   (let ((p (pop jedi:goto-stack)))
;;     (if p (progn
;;             (switch-to-buffer (nth 0 p))
;;             (goto-char (nth 1 p))))))
;;------------------------------------------------------------------------------
