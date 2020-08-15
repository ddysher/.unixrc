;;------------------------------------------------------------------------------
;; Initialize elpa package management system
;;------------------------------------------------------------------------------
(require 'package)

;; Package archives source
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; 'melpa' is sufficient for the most cases, so comment out the following sources.
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives  ("gnu" . "http://elpa.gnu.org/packages/"))

;; On-demand installation of packages. Install given PACKAGE, optionally
;; requiring MIN-VERSION. If NO-REFRESH is non-nil, the available package
;; lists will not be re-downloaded in order to locate PACKAGE.
(defun require-package (package &optional min-version no-refresh)
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (progn
          (package-refresh-contents)    ; refresh anyway
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(package-initialize)

(provide 'init-elpa)
