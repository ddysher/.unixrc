;;------------------------------------------------------------------------------
;; Provide org mode, built-in with emacs, but use package from MELPA.
;;
;; Frequently used command:
;;   M-RET          org-meta-return
;;   C-c C-t        org-todo
;;   C-c C-o        org-open-at-point
;;   Shift-TAB      org-shifttab
;;------------------------------------------------------------------------------
(require-package 'org)
(require 'org)


(setq org-log-done t)


(provide 'init-org-mode)
