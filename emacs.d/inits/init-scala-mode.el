;;------------------------------------------------------------------------------
;; Provide scala mode.
;;------------------------------------------------------------------------------
;; Installation (ensime is deprecated)
;;
;; http://ensime.github.io/editors/emacs/install/
;;
;;------------------------------------------------------------------------------
;; IDE
;;
;; Ensime provides integrated development environment for scala. To use ensime,
;; run "sbt ensimeConfig" in project root, and "M-x ensime" to connect to the
;; external java process.
;;------------------------------------------------------------------------------
(require-package 'scala-mode)
(require-package 'sbt-mode)
;; Both are autoloaded by their packages for .scala / .sbt files.

(provide 'init-scala-mode)
