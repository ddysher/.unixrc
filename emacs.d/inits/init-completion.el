;;------------------------------------------------------------------------------
;; Completion stack flow:
;;   sources      -> completion candidates
;;   matching     -> filter candidates
;;   presentation -> show candidates in minibuffer or buffer
;;   actions      -> act on selected candidates
;;
;; Sources:
;;   cape       - extra completion-at-point sources (file, dabbrev, etc.)
;;   tempel     - snippets exposed through completion-at-point
;;   consult    - commands that produce completing-read candidates
;;
;; Matching:
;;   orderless  - space-separated matching style for completion
;;
;; Presentation:
;;   vertico    - minibuffer completion UI
;;   corfu      - in-buffer completion-at-point UI
;;   marginalia - minibuffer annotations
;;
;; Actions:
;;   embark     - contextual actions on completion candidates
;;   embark-consult - consult preview support for Embark collect buffers
;;
;; Some keybindings are defined in init-global-keys.el.
;; In minibuffer, type SPC-separated tokens for orderless filtering.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Matching
;;------------------------------------------------------------------------------
;; Orderless defines how typed patterns match completion candidates.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  ;; Use basic style for file paths so partial-path completion works.
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;------------------------------------------------------------------------------
;; Sources - Minibuffer Candidates
;;------------------------------------------------------------------------------
;; Consult provides richer completing-read commands for buffers, search, etc.
(use-package consult
  :defer t)

;;------------------------------------------------------------------------------
;; Sources - Completion-At-Point Candidates
;;------------------------------------------------------------------------------
;; Cape provides extra completion-at-point sources.
;; These are appended after any mode-specific capfs (e.g. eglot), so LSP
;; completions take priority and cape provides fallback sources.
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

;; tempel-complete is a capf, so snippets appear inline in the Corfu popup.
;; TAB / S-TAB navigate between placeholders after expanding.
;; Templates are defined in ~/.emacs.d/user-data/templates.
(use-package tempel
  :custom
  (tempel-path (expand-file-name "templates" user-data-dir))
  :init
  (add-hook 'completion-at-point-functions #'tempel-complete)
  :bind
  (:map tempel-map
   ("TAB"       . tempel-next)
   ("<backtab>" . tempel-previous)))

;;------------------------------------------------------------------------------
;; Presentation
;;------------------------------------------------------------------------------
;; Vertico displays minibuffer completion candidates in a vertical list.
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-count 20))

;; Marginalia adds minibuffer annotations such as keybindings, docs, and file metadata.
(use-package marginalia
  :init
  (marginalia-mode))

;; Corfu displays in-buffer completion-at-point candidates in a popup near point.
(use-package corfu
  :custom
  (corfu-auto t)                  ; show popup automatically
  (corfu-auto-delay 0.3)          ; delay before popup appears
  (corfu-auto-prefix 3)           ; minimum prefix length to trigger completion
  (corfu-cycle t)                 ; wrap around candidates
  (corfu-quit-no-match 'separator); keep popup open even with no match (for orderless)
  :init
  (global-corfu-mode))

;;------------------------------------------------------------------------------
;; Candidate Actions
;;------------------------------------------------------------------------------
;; Embark runs context-sensitive actions on the current candidate or symbol.
(use-package embark
  :defer t
  :custom
  (embark-indicators '(embark-minimal-indicator
                        embark-highlight-indicator
                        embark-isearch-highlight-indicator)))

;; Embark-Consult lets Embark collect buffers use Consult live preview.
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-completion)
