;;------------------------------------------------------------------------------
;; Provide protobuf-mode, a major for editing .proto file, managed by elpa.
;;------------------------------------------------------------------------------
(require-package 'protobuf-mode)
(require 'protobuf-mode)

(defun protobuf-mode-custom-hook ()
  (setq c-basic-offset universal-indent-size))

(add-hook 'protobuf-mode-hook 'protobuf-mode-custom-hook)

(provide 'init-protobuf-mode)
