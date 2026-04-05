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
(use-package scala-mode :defer t)
(use-package sbt-mode :defer t)

(provide 'init-scala-mode)
