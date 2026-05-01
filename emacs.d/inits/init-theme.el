;;------------------------------------------------------------------------------
;; Theme and font defaults
;; - Theme: primarily uses doomemacs themes.
;; - Fonts: set up primary font, icon fallback setup, etc.
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Theme and modeline configuration
;;------------------------------------------------------------------------------
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-unicode-number nil)
  (doom-modeline-buffer-encoding 'nondefault))

;; Render modeline icons with `Symbols Nerd Font Mono' (natural double-width
;; glyphs, no overlap) while keeping SauceCodePro Nerd Font everywhere else.
;; `doom-modeline-propertize-icon' is the chokepoint: every modeline icon
;; (via `doom-modeline-icon', buffer-file-icon, vcs, lsp, checker, …) passes
;; through it and gets re-propertized with an explicit `:family'. Force that
;; family to the symbols font. Icons produced elsewhere (dired, completions)
;; never call this function and keep the global `nerd-icons-font-family'.
(defun laura/doom-modeline-restyle-icon (icon)
  "Force modeline ICON to use Symbols Nerd Font Mono at 0.8× size.
Idempotent via a `laura/modeline-icon' text property so the inactive-window
re-propertize pass doesn't compound the height multiplier."
  (if (or (not (stringp icon))
          (string-empty-p icon)
          (get-text-property 0 'laura/modeline-icon icon))
      icon
    (let ((face (get-text-property 0 'face icon)))
      (propertize (substring-no-properties icon)
                  'face `(:inherit ,face
                          :family "Symbols Nerd Font Mono"
                          :height 0.8)
                  'display '(raise 0.1)
                  'laura/modeline-icon t))))

(with-eval-after-load 'doom-modeline-core
  (advice-add 'doom-modeline-propertize-icon :filter-return
              #'laura/doom-modeline-restyle-icon))

;;------------------------------------------------------------------------------
;; Font and symbols configuration
;;------------------------------------------------------------------------------
(use-package nerd-icons
  :config ; default: "Symbols Nerd Font Mono", set via nerd-icons-font-family
  (setq nerd-icons-font-family "SauceCodePro Nerd Font"))

;; macOS font stack. SauceCodePro is a Nerd Font so terminal and Emacs render
;; the same icon glyphs. Specific Unicode blocks are pinned to monospace fonts
;; before CoreText proportional fallbacks can fire — proportional metrics cause
;; vterm row jumps. Unifont covers everything else as the ultimate fallback.
;; > brew install --cask font-source-code-pro font-noto-sans-nano-cjk-sc
;;     font-symbols-only-nerd-font font-gnu-unifont
(when *darwin*
  ;; Relative size overrides
  (setq face-font-rescale-alist '(("Unifont" . 0.6)))

  ;; 1. Set the standard, unpatched Source Code Pro as primary font.
  (set-frame-font "SauceCodePro Nerd Font-12")

  ;; 2. Set Chinese font Noto Sans Mono CJK SC.
  (set-fontset-font t 'han      "LXGW WenKai")
  (set-fontset-font t 'cjk-misc "LXGW WenKai")

  ;; 3. Specific unicode region overrides for glyph.
  (set-fontset-font t '(#x25A0 . #x25FF) (font-spec :family "Menlo"))   ; Geometric Shapes
  (set-fontset-font t '(#x2700 . #x27BF) (font-spec :family "Menlo"))   ; Dingbats
  (set-fontset-font t '(#x2300 . #x23FF) (font-spec :family "Unifont")) ; Misc Technical

  ;; 4. Final fallback for anything not caught above.
  (set-fontset-font t 'unicode (font-spec :family "Unifont") nil 'append))

;; Linux font stack.
(when *linux* (set-frame-font "Hack-9.5"))

(provide 'init-theme)
