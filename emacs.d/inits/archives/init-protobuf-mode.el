;;------------------------------------------------------------------------------
;; Provide protobuf-mode, a major for editing .proto file, managed by elpa.
;;------------------------------------------------------------------------------
(use-package protobuf-mode
  :hook (protobuf-mode . (lambda () (setq c-basic-offset default-indent-size))))

(provide 'init-protobuf-mode)
