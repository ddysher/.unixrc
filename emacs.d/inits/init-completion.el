;;------------------------------------------------------------------------------
;; Completion: minibuffer (Vertico stack) + in-buffer (Corfu + Cape)
;;
;; Minibuffer completion:
;;   vertico    - vertical completion UI in the minibuffer
;;   orderless  - space-separated fuzzy/regexp completion style (shared)
;;   marginalia - rich annotations next to candidates (docstrings, types, etc.)
;;   consult    - enhanced commands built on completing-read
;;   embark     - contextual actions on any completion candidate
;;
;; In-buffer completion:
;;   corfu      - popup completion UI for completion-at-point
;;   cape       - completion-at-point extensions (file, dabbrev, etc.)
;;   tempel     - lightweight snippet system; templates in ~/.emacs.d/templates
;;
;; LSP / Eglot (base config; language servers registered in language files):
;;   eglot      - built-in LSP client (Emacs 29+), provides capf for corfu
;;
;; Keybindings are defined in init-keys.el.
;; In minibuffer, type SPC-separated tokens for orderless filtering.
;;------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; Minibuffer completion (Vertico stack)
;;------------------------------------------------------------------------------
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-count 20))

;; orderless is shared between minibuffer and corfu (in-buffer) completion.
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


;;------------------------------------------------------------------------------
;; In-buffer completion (Corfu + Cape)
;;------------------------------------------------------------------------------
(use-package corfu
  :custom
  (corfu-auto t)                  ; show popup automatically
  (corfu-auto-delay 0.2)          ; delay before popup appears
  (corfu-auto-prefix 2)           ; minimum prefix length to trigger completion
  (corfu-cycle t)                 ; wrap around candidates
  (corfu-quit-no-match 'separator); keep popup open even with no match (for orderless)
  :init
  (global-corfu-mode))

;; Cape provides extra completion-at-point sources.
;; These are appended after any mode-specific capfs (e.g. eglot), so LSP
;; completions take priority and cape provides fallback sources.
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t))


;;------------------------------------------------------------------------------
;; Snippets (Tempel)
;;------------------------------------------------------------------------------
;; tempel-complete is a capf, so snippets appear inline in the Corfu popup.
;; TAB / S-TAB navigate between placeholders after expanding.
;; Templates are defined in ~/.emacs.d/user-data/templates.
(use-package tempel
  :custom
  (tempel-path "~/.emacs.d/user-data/templates")
  :init
  (add-to-list 'completion-at-point-functions #'tempel-complete t)
  :bind
  (:map tempel-map
   ("TAB"       . tempel-next)
   ("<backtab>" . tempel-previous)))


;;------------------------------------------------------------------------------
;; Eglot (LSP client, Emacs 29+)
;;------------------------------------------------------------------------------
;; Base configuration only. Language-specific server programs are registered
;; in the respective language files using with-eval-after-load.
;; Eglot provides completion-at-point → corfu consumes it for LSP completions.
(use-package eglot
  :ensure nil
  :defer t)

(provide 'init-completion)
