 ;;------------------------------------------------------------------------------
;; Initialize elpa package management system and use-package
;;------------------------------------------------------------------------------
(require 'package)
(require 'use-package)       ; use-package is built-in since Emacs 29.
(setq use-package-always-ensure t)

;; Package archives source
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives  '("gnu" . "http://elpa.gnu.org/packages/"))

(package-initialize)

;; Auto-refresh quickstart cache after installing or deleting packages.
(advice-add 'package-install :after
            (lambda (&rest _) (package-quickstart-refresh)))
(advice-add 'package-delete :after
            (lambda (&rest _) (package-quickstart-refresh)))

(provide 'init-elpa-packages)
