;;------------------------------------------------------------------------------
;; Theme and font defaults
;;------------------------------------------------------------------------------
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-one t))

;; macOS uses a Nerd Font so terminal and Emacs render the same icon glyphs.
;; When using a non-Nerd Font, add a PUA fallback such as:
;;   (set-fontset-font t '(#xE000 . #xF8FF) "Symbols Nerd Font Mono")
;; > brew install --cask font-sauce-code-pro-nerd-font font-symbols-only-nerd-font
(cond (*darwin* (set-frame-font "SauceCodePro Nerd Font-12")))
(cond (*linux*  (set-frame-font "Hack-9.5")))

;; In terminal mode, use a thin Unicode line for window splits.
;; Themes often set a background on vertical-border, making it look like a thick bar.
(unless (display-graphic-p)
  (set-face-attribute 'vertical-border nil :background 'unspecified :foreground "#435558")
  (unless standard-display-table
    (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│)))

(provide 'init-theme)
