;;------------------------------------------------------------------------------
;; Provide cc mode, cc-mode is built-in emacs mode, for c, c++, java, etc.
;;
;; Browsing c/c++ source code requires a bit of configuration; right now, we only
;; configure tags. A bit of terms:
;; - ctags is a command to generate 'tags' file which is the tag file for vi.
;;   Exuberant ctags extends ctags, and Universal ctags continues its development.
;; - etags is a command to generate 'TAGS' file which is the tag file for Emacs.
;; - gtags is a command to generate tag files for GLOBAL (GTAGS, GRTAGS, GPATH).
;;   GNU GLOBAL is a source code tagging system.
;; - ggtags is emacs frontend to GNU Global source code tagging system.
;;------------------------------------------------------------------------------
(require-package 'ggtags)


(defun cc-mode-custom-hook ()
  (ggtags-mode 1)
  (local-set-key (kbd "M-.") 'ggtags-find-tag-dwim)
  (local-set-key (kbd "M-,") 'ggtags-prev-mark)
  (setq c-basic-offset universal-indent-size)
  (setq c-default-style "bsd")
  (setq comment-start "//" comment-end "")
  (local-set-key "\C-m" 'newline-and-indent)) ; indent next line properly

(add-hook 'c-mode-hook 'cc-mode-custom-hook)
(add-hook 'c++-mode-hook 'cc-mode-custom-hook)


(provide 'init-cc-mode)
