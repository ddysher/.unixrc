;;------------------------------------------------------------------------------
;; Initialize elpa package management system and use-package
;;------------------------------------------------------------------------------
(require 'package)

;; Package archives source
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives  '("gnu" . "http://elpa.gnu.org/packages/"))

;; Auto-refresh quickstart cache after installing or deleting packages.
(advice-add 'package-install :after
            (lambda (&rest _) (package-quickstart-refresh)))
(advice-add 'package-delete :after
            (lambda (&rest _) (package-quickstart-refresh)))

;; use-package is built-in since Emacs 29.
(require 'use-package)
(setq use-package-always-ensure t)

;; Keep require-package for backward compatibility with unconverted files.
(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (progn
          (package-refresh-contents)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(provide 'init-elpa-packages)
