;;------------------------------------------------------------------------------
;; Provide w3m, w3m is elpa managed package.  This mode depends on external
;; program, to use this mode, host system must have 'w3m' command installed.
;; E.g. brew install w3m, sudo apt-get install w3m
;;------------------------------------------------------------------------------
(require-package 'w3m)
(require 'w3m)


(defun w3m-mode-custom-hook ()
  (local-set-key [up] 'previous-line)
  (local-set-key [down] 'next-line)
  (local-set-key [C-up] 'w3m-previous-anchor)
  (local-set-key [C-down] 'w3m-next-anchor)
  (local-set-key [left] 'w3m-view-previous-page)
  (local-set-key [right] 'w3m-view-next-page)
  (local-set-key (kbd "H") 'w3m-view-previous-page)
  (local-set-key (kbd "L") 'w3m-view-next-page)
  (local-set-key (kbd "M-n") 'scroll-up-in-place)
  (local-set-key (kbd "C-c b") 'w3m-browse-url))

(add-hook 'w3m-mode-hook 'w3m-mode-custom-hook)

(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-use-cookies t
      w3m-cookie-accept-bad-cookies t
      w3m-home-page "http://www.google.com"
      w3m-search-default-engine "g"
      w3m-default-display-inline-images t)


(provide 'init-w3m)
