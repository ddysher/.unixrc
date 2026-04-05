;;------------------------------------------------------------------------------
;; Python mode configuration: Eglot + Pyright + python-ts-mode
;;------------------------------------------------------------------------------
;; Uses eglot (built-in Emacs 29+) with pyright as the language server, and
;; python-ts-mode (tree-sitter) for superior syntax highlighting/indentation.
;; Eglot integrates with standard Emacs infrastructure: xref for navigation,
;; flymake for diagnostics, eldoc for docs, capf for completions (via company).
;;
;; Dependencies:
;;   pyright - language server: npm install -g pyright (or: pip install pyright)
;;   python tree-sitter grammar - install once via:
;;     M-x treesit-install-language-grammar -> python
;;
;; python-ts-mode is the tree-sitter variant of python-mode (Emacs 29+).
;; major-mode-remap-alist transparently redirects python-mode -> python-ts-mode,
;; so all hooks and auto-mode-alist entries for python-mode still fire.
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
;; PyEnv
;;
;; The python version inside emacs is determined while launching emacs, which
;; means changing python version using command like `pyenv global 3.6.7` doesn't
;; work (e.g. if a package is only installed in python version 2.7, then code
;; jump doesn't work). To change version in emacs, use:
;;   M-x pyenv-mode-set
;; To unset change, run:
;;   M-x pyenv-mode-unset
;; To check current python version, run:
;;   M-x run-python
;;
;; If a file is already opened with another python version, try revert-buffer.
;;------------------------------------------------------------------------------

;; Remap python-mode to python-ts-mode for tree-sitter powered highlighting.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; Eglot is built-in (Emacs 29+). Pyright is auto-detected if installed;
;; explicitly listed here to prefer it over the default pylsp.
(use-package eglot
  :ensure nil
  :hook (python-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

(use-package pyenv-mode
  :if (executable-find "pyenv")
  :hook (after-init . pyenv-mode))

(defun python-mode-custom-hook ()
  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key (kbd "C-c d") 'eldoc-doc-buffer)
  (setq python-indent-offset universal-indent-size))
;; M-. (xref-find-definitions) and M-, (xref-go-back) are bound by eglot via xref.

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
