;;------------------------------------------------------------------------------
;; Provide geben mode, although gun-mode is managed by elpa, there is some
;; conflictions between geben and other packages (e.g. erc), due to the
;; definition of (defadvice open-network-stream ....). I fix that problem and
;; geben into site-lisp directory, so it is not managed by melpa. geben-mode
;; is mainly used use for debugging PHP code.
;;
;; Some basic command:
;;   SPC geben-step-again
;;   > geben-set-redirect
;;   ? geben-mode-help
;;   B geben-breakpoint-menu
;;   U geben-clear-breakpoints
;;   b geben-set-breakpoint-line
;;   c geben-run-to-cursor
;;   d geben-show-backtrace
;;   e geben-eval-expression
;;   g geben-run
;;   i geben-step-into
;;   o geben-step-over
;;   q geben-stop
;;   r geben-step-out
;;   t geben-show-backtrace
;;   u geben-unset-breakpoint-line
;;   v geben-display-context
;;   w geben-where
;;
;;   C-x SPC geben-set-breakpoint-line
;;
;;   C-c C-c geben-run
;;   C-c C-d geben-unset-breakpoint-line
;;   C-c C-l geben-where
;;   C-c C-n geben-step-over
;;   C-c C-s geben-step-into
;;   C-c C-t geben-set-breakpoint-line
;;   C-c b geben-show-breakpoint-list
;;   C-c f geben-find-file
;;   C-c p geben-toggle-pause-at-entry-line-flag
;;   C-u t change redirection mode
;; Usage:
;;   M-x geben, then emacs is listening on port 9000, we need to append
;;   ?XDEBUG_SESSION_START=1 on any url to start debugging. In addition, there
;;   are some other setting for php.  See http://chen-shan.net/?p=678
;;------------------------------------------------------------------------------
(require 'dbgp)
(require 'geben)


(provide 'init-geben-mode)
