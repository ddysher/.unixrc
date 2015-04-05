;;------------------------------------------------------------------------------
;; Provide cc mode, cc-mode is built-in emacs mode, for c, c++, java, etc.
;; ggtags is a pakcage for c/c++ tagging. It uses GNU GLOBAL as its tagging
;; system.  To use ggtags, first install GNU GLOBAL. For a project, use:
;; ggtags-create-tags to create GTAGS first.
;;------------------------------------------------------------------------------
(require-package 'ggtags)


(defun cc-mode-custom-hook ()
  (ggtags-mode 1)
  (local-set-key (kbd "M-.") 'ggtags-find-definition)
  (local-set-key (kbd "M-,") 'ggtags-prev-mark)
  (setq c-basic-offset universal-indent-size)
  (setq c-default-style "bsd")
  (local-set-key "\C-m" 'newline-and-indent)) ; indent next line properly

(add-hook 'c-mode-hook 'cc-mode-custom-hook)
(add-hook 'c++-mode-hook 'cc-mode-custom-hook)


(provide 'init-cc-mode)
