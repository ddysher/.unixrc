;;------------------------------------------------------------------------------
;; Provide python mode, python-mode is built-in emacs mode.
;;
;; Jedi is a Python It also helps you to find information about Python objects,
;; such as docstring, function arguments and code location. To enable jedi,
;; following packages are needed in emacs:
;;   EPC, deferred.el, auto-complete and python-environment.el.
;; Also, following command and packages need to be installed in host OS:
;;   virtualenv, jedi, python-epc and argparse.
;;
;; Install Python server (jediepcserver.py) by running
;;   M-x jedi:install-server in Emacs
;;------------------------------------------------------------------------------
(require-package 'epc)
(require-package 'deferred)
(require-package 'python-environment)

(require-package 'jedi)
(require 'jedi)


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


(provide 'init-python-mode)
