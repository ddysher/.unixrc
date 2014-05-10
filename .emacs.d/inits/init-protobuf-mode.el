;;------------------------------------------------------------------------------
;; Provide protobuf-mode, a major for editing .proto file.
;;------------------------------------------------------------------------------
(require-package 'protobuf-mode)
(require 'protobuf-mode)


(defun my-protobuf-mode-hook ()
  (setq c-basic-offset universal-indent-size))

(add-hook 'protobuf-mode-hook 'my-protobuf-mode-hook)


(provide 'init-protobuf-mode)
