;;------------------------------------------------------------------------------
;; Provide org mode, built-in with emacs, but use package from MELPA.
;; NOTE (require-package 'org) doesn't work correctly, need to install manually.
;;
;; Common commands:
;;   M-Left     Promote one level for current heading.
;;   M-Right    Demote one level for current heading.
;;   M-Return   Continue to next line with the same heading level.
;;   C-c C-t    Toggle TODO / DONE
;;   C-c C-c    Add tag to heading.
;;   C-c C-s    Add scheduled time.
;;   C-c C-d    Add deadline time.
;;   C-c C-x C-a   Archive heading.
;;   C-c '      Edit source code.
;;------------------------------------------------------------------------------
(require-package 'org)
(require 'org)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-return-follows-link t)
(setq org-log-done t)
(setq org-agenda-files '("~/org/general.org"))
(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t) ; enable native source code hightlight

(defun org-agenda--mode-custom-hook ()
  (text-scale-decrease 1))         ; use smaller size for agenda mode.

(add-hook 'org-agenda-mode-hook 'org-agenda--mode-custom-hook)

(provide 'init-org-mode)
