;;------------------------------------------------------------------------------
;; Provide multi-vterm mode, enhanced vterm-mode in emacs
;;------------------------------------------------------------------------------
(use-package vterm
  :defer t
  :init
  (blink-cursor-mode -1)
  :config
  (defun vterm-mode-custom-hook ()
    (define-key vterm-mode-map [f1] #'find-file)
    (define-key vterm-mode-map [f3] #'other-window)
    (define-key vterm-mode-map [f4] #'multi-vterm-dedicated-open)
    (define-key vterm-mode-map [f5] #'multi-vterm)
    (define-key vterm-mode-map [f8] #'register-to-point)
    (define-key vterm-mode-map (kbd "C-o")  #'switch-to-buffer)
    (define-key vterm-mode-map (kbd "C-q")  #'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "M-[")  #'multi-vterm-prev)
    (define-key vterm-mode-map (kbd "M-]")  #'multi-vterm-next)
    ;; Claude Code emits U+23FA (BLACK CIRCLE FOR RECORD) in its spinner.
    ;; No Nerd Font covers it, so Emacs falls back to STIX Two Math whose
    ;; taller metrics make vterm rows jump. Substitute it to ◉ via the
    ;; buffer-local display table.
    (when buffer-display-table
      (aset buffer-display-table ?⏺ (vector ?◉)))
    ;; Claude Code's banner pads with U+00A0 (NO-BREAK SPACE). Emacs's
    ;; built-in `nobreak-space' face highlights every NBSP, which shows up
    ;; as a cyan bar under the banner. Disable it locally.
    (setq-local nobreak-char-display nil))

  ;; Claude Code hides the terminal cursor via DECTCEM (\e[?25l) while
  ;; rendering its Ink UI. vterm-module.c honors that by setting the
  ;; buffer-local `cursor-type' to nil (see term_movecursor's
  ;; cursor_visible == false branch). `vterm-copy-mode' then inherits that
  ;; invisible cursor, so point moves but nothing is drawn. Force a visible
  ;; cursor on copy-mode entry; when copy-mode exits, the next frame from
  ;; vterm will re-sync `cursor-type' to whatever the running app wants.
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (when vterm-copy-mode
                (setq-local cursor-type 'box))))

  ;; Claude Code's thinking-mode spinner cycles through Dingbats decorative
  ;; asterisks. By default, Emacs falls back to proportional fonts like Arial
  ;; Unicode MS, whose taller ascent+descent make the vterm row jump as the
  ;; spinner cycles. Menlo ships with the whole Dingbats block with monospace-
  ;; compatible metrics, so pin Menlo as the Dingbats font. Using a
  ;; 'font-spec' and *replacing* (no 'prepend) makes this stick on macOS —
  ;; 'prepend with a bare string gets bypassed by the CoreText fallback.
  (when *darwin*
    (set-fontset-font t '(#x2700 . #x27BF) (font-spec :family "Menlo")))

  (add-hook 'vterm-mode-hook 'vterm-mode-custom-hook))

(use-package multi-vterm
  :after vterm
  :commands (multi-vterm multi-vterm-dedicated-open multi-vterm-prev multi-vterm-next))

(provide 'init-multi-vterm)
