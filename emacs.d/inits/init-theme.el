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
;;
;; > brew install --cask font-sauce-code-pro-nerd-font font-symbols-only-nerd-font
(cond (*darwin* (set-frame-font "SauceCodePro Nerd Font-12")))
(cond (*linux*  (set-frame-font "Hack-9.5")))


(provide 'init-theme)
