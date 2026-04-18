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
  (doom-modeline-minor-modes t))

;; macOS uses a Nerd Font so terminal and Emacs render the same icon glyphs.
;; When using a non-Nerd Font, add a PUA fallback such as:
;;   (set-fontset-font t '(#xE000 . #xF8FF) "Symbols Nerd Font Mono")
;; > brew install --cask font-sauce-code-pro-nerd-font font-symbols-only-nerd-font
(cond (*darwin* (set-frame-font "SauceCodePro Nerd Font-12")))
(cond (*linux*  (set-frame-font "Hack-9.5")))

;; Chinese fonts: Noto Sans CJK SC on all systems.
;; > brew install --cask font-noto-sans-cjk-sc
(set-fontset-font t 'han "Noto Sans CJK SC")
(set-fontset-font t 'cjk-misc "Noto Sans CJK SC")

(provide 'init-theme)
