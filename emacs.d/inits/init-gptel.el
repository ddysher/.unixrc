;;------------------------------------------------------------------------------
;; Provide gptel mode, managed by melpa.
;;------------------------------------------------------------------------------
(require-package 'gptel)

;; Defer gptel until first use (M-x gptel).
(with-eval-after-load 'gptel
  (setq gptel-model 'deepseek-chat
        gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (lambda ()
                 (let ((api-key-file (expand-file-name "~/.config/.deepseek.key")))
                   (unless (file-exists-p api-key-file)
                     (error "DeepSeek API key file not found at %s" api-key-file))
                   (with-temp-buffer
                     (insert-file-contents api-key-file)
                     (string-trim (buffer-string)))))
          :models '(deepseek-chat))))

(provide 'init-gptel)
