;;------------------------------------------------------------------------------
;; gptel is a simple, extensible LLM client for Emacs.
;;------------------------------------------------------------------------------

(require 'subr-x)

(defun laura/gptel-api-key (file)
  "Read an API key from FILE."
  (let ((api-key-file (expand-file-name file)))
    (unless (file-exists-p api-key-file)
      (error "API key file not found at %s" api-key-file))
    (with-temp-buffer
      (insert-file-contents api-key-file)
      (string-trim (buffer-string)))))

(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :config
  (gptel-make-openai "OpenAI"
    :host "api.openai.com"
    :endpoint "/v1/chat/completions"
    :stream t
    :key (lambda () (laura/gptel-api-key "~/.secrets/openai"))
    :models '(gpt-5.5 gpt-5.4 gpt-5.4-mini gpt-5.4-nano))

  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (lambda () (laura/gptel-api-key "~/.secrets/deepseek"))
    :models '(deepseek-chat deepseek-reasoner))

  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(gemma4:26b))

  (setq gptel-backend (gptel-get-backend "Ollama")
        gptel-model 'gemma4:26b))

(provide 'init-gptel)
