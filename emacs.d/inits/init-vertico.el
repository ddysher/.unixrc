;;------------------------------------------------------------------------------
;; Vertico stack: modern minibuffer completion UI.
;;
;; Packages:
;;   vertico    - vertical completion UI in the minibuffer
;;   orderless  - space-separated fuzzy/regexp completion style
;;   marginalia - rich annotations next to candidates (docstrings, types, etc.)
;;   consult    - enhanced commands built on completing-read
;;   embark     - contextual actions on any completion candidate
;;
;; Keybindings are defined in init-keys.el.
;; In minibuffer, type SPC-separated tokens for orderless filtering.
;;------------------------------------------------------------------------------

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-count 20))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;; Use basic style for file paths so partial-path completion works.
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult)

(use-package embark
  :custom
  (embark-indicators '(embark-minimal-indicator
                        embark-highlight-indicator
                        embark-isearch-highlight-indicator)))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Persist minibuffer history across sessions (used by vertico/consult).
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(provide 'init-vertico)
