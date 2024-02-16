;;------------------------------------------------------------------------------
;; Provide python mode, python-mode is built-in emacs mode.
;;------------------------------------------------------------------------------
;; Autocompletion, Code Jump
;;
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
;; implementation of EPC is epc.el and Python side is python-epc. There is a
;; process running (EPC server side), e.g.
;;   ~/.emacs.d/.python-environments/default/bin/python ~/.emacs.d/.python-environments/default/bin/jediepcserver
;;
;;------------------------------------------------------------------------------
;; Debugging
;;
;; Python debugging uses gun mode, gun-mode is built-in emacs mode. This mode
;; enables debugging python code using pdb.
;;
;; Usage:
;;   M-x pdb, then input the script path
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
(require-package 'epc)
(require-package 'deferred)
(require-package 'python-environment)
(require-package 'jedi)
(require-package 'pyenv-mode)
(require 'jedi)
(require 'gud)

;; This method will be registered as a python mode hook, runs
;; every time python file is opened.
(defun python-mode-custom-hook ()
  (local-set-key "\C-m" 'newline-and-indent)
  (local-set-key (kbd "M-.") 'jedi:jump-to-definition)
  (local-set-key (kbd "M-,") 'jedi:jump-back)
  (local-set-key (kbd "C-c d") 'jedi:show-doc)
  (local-set-key (kbd "C-<tab>") 'jedi:complete)
  (setq python-indent-offset universal-indent-size))

(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))
(add-hook 'python-mode-hook 'python-mode-custom-hook)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(when *darwin*
  (pyenv-mode))

(defvar jedi:goto-stack '())

(defun jedi:jump-to-definition ()
  (interactive)
  (add-to-list 'jedi:goto-stack
               (list (buffer-name) (point)))
  (jedi:goto-definition))

(defun jedi:jump-back ()
  (interactive)
  (let ((p (pop jedi:goto-stack)))
    (if p (progn
            (switch-to-buffer (nth 0 p))
            (goto-char (nth 1 p))))))

(defun gud-mode-custom-hook ()
  (define-key gud-mode-map '[f9] 'gud-cont)
  (define-key gud-mode-map '[f10] 'gud-next)
  (define-key gud-mode-map '[f11] 'gud-step)
  (define-key gud-mode-map '[f12] 'gud-break))

(add-hook 'gud-mode-hook 'gud-mode-custom-hook)

(provide 'init-python-mode)
