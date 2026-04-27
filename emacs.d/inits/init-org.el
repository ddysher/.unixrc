;;------------------------------------------------------------------------------
;; Configure built-in Org mode.
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

(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :config
  (setq org-return-follows-link t)
  (setq org-log-done t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)

  (defun org-agenda--mode-custom-hook ()
    (text-scale-decrease 1))
  (add-hook 'org-agenda-mode-hook 'org-agenda--mode-custom-hook))

(provide 'init-org)
