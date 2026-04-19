;;------------------------------------------------------------------------------
;; Theme and font defaults
;;------------------------------------------------------------------------------
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-unicode-number nil))

(cond (*linux* (set-frame-font "Hack-9.5")))

;; macOS font stack. SauceCodePro is a Nerd Font so terminal and Emacs render
;; the same icon glyphs. Specific Unicode blocks are pinned to monospace fonts
;; before CoreText proportional fallbacks can fire — proportional metrics cause
;; vterm row jumps. Unifont covers everything else as the ultimate fallback.
;; > brew install --cask font-sauce-code-pro-nerd-font font-noto-sans-nano-cjk-sc
;;     font-gnu-unifont font-symbols-only-nerd-font (for fallback)
(when *darwin*
  ;; Relative size overrides so these fonts scale with text-scale-adjust.
  ;; Ratios are relative to SauceCodePro 12pt (CJK: 10/12, Unifont: 7/12).
  (setq face-font-rescale-alist
        '(("Noto Sans Mono CJK SC" . 0.85)
          ("Unifont" . 0.60)))
  (set-frame-font "SauceCodePro Nerd Font-12")
  (set-fontset-font t 'han      "Noto Sans Mono CJK SC")
  (set-fontset-font t 'cjk-misc "Noto Sans Mono CJK SC")
  (set-fontset-font t '(#x25A0 . #x25FF) (font-spec :family "Menlo")) ; Geometric Shapes
  (set-fontset-font t '(#x2700 . #x27BF) (font-spec :family "Menlo")) ; Dingbats
  (set-fontset-font t '(#x2300 . #x23FF) (font-spec :family "Unifont")) ; Misc Technical
  (set-fontset-font t 'unicode (font-spec :family "Unifont") nil 'append)) ; Final fallback

(provide 'init-theme)
