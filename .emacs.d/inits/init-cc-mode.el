;;------------------------------------------------------------------------------
;; Provide cc mode, cc-mode is built-in emacs mode, for c, c++, java, etc.
;;
;; ggtags is a pakcage for c/c++ tagging. It uses GNU GLOBAL as its tagging
;; system.  To use ggtags, first install GNU GLOBAL. For a project, use:
;; ggtags-create-tags to create GTAGS first.  Use M-x ggtags-mode to enable
;; tags; or put (ggtags-mode 1) in cc-mode-custoe-hook().  ggtags-mode is
;; disabled by default as it requires GTAGS file for evary file, annoying.
;; A better approach would be to enable it in certain directory.
;;------------------------------------------------------------------------------
(require-package 'ggtags)


(defun cc-mode-custom-hook ()
  (local-set-key (kbd "M-.") 'ggtags-find-definition)
  (local-set-key (kbd "M-,") 'ggtags-prev-mark)
  (setq c-basic-offset universal-indent-size)
  (setq c-default-style "bsd")
  (setq comment-start "//" comment-end "")
  (local-set-key "\C-m" 'newline-and-indent)) ; indent next line properly

(add-hook 'c-mode-hook 'cc-mode-custom-hook)
(add-hook 'c++-mode-hook 'cc-mode-custom-hook)


(provide 'init-cc-mode)
