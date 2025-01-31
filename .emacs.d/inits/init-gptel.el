;;------------------------------------------------------------------------------
;; Provide gptel mode, managed by melpa.
;;------------------------------------------------------------------------------
(require-package 'gptel)
(require 'gptel)

(setq gptel-model 'deepseek-chat
      gptel-backend
      (gptel-make-openai "DeepSeek"
        :host "api.deepseek.com"
        :endpoint "/chat/completions"
        :stream t
        :key (lambda ()       ; read the key when DeepSeek is invoked.
               (let ((api-key-file (expand-file-name "~/.config/.deepseek.key")))
                 (unless (file-exists-p api-key-file)
                   (error "DeepSeek API key file not found at %s" api-key-file))
                 (with-temp-buffer
                   (insert-file-contents api-key-file)
                   (string-trim (buffer-string)))))
        :models '(deepseek-chat)))

(provide 'init-gptel)
