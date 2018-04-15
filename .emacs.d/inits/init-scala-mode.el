;;------------------------------------------------------------------------------
;; Provide scala mode.
;;------------------------------------------------------------------------------
;; Installation
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
(require-package 'ensime)

(require 'scala-mode)
(require 'sbt-mode)
(require 'ensime)

(provide 'init-scala-mode)
