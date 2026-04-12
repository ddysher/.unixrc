;;------------------------------------------------------------------------------
;; Multi-vterm: enhanced terminal emulation with workarounds for
;; full-screen TUI apps (Claude Code, Codex) and minibuffer interactions.
;;------------------------------------------------------------------------------
(use-package vterm
  :defer t
  :init
  (blink-cursor-mode -1)
  :config
  (defun vterm-mode-custom-hook ()
    ;; Keybindings for both vterm-mode and vterm-copy-mode.
    (define-key vterm-mode-map [f1] #'find-file)
    (define-key vterm-mode-map [f3] #'other-window)
    (define-key vterm-mode-map [f4] #'multi-vterm-dedicated-open)
    (define-key vterm-mode-map [f5] #'multi-vterm)
    (define-key vterm-mode-map [f8] #'register-to-point)
    (define-key vterm-mode-map (kbd "C-o")  #'switch-to-buffer)
    (define-key vterm-mode-map (kbd "C-q")  #'vterm-copy-mode)
    (define-key vterm-mode-map (kbd "M-[")  #'multi-vterm-prev)
    (define-key vterm-mode-map (kbd "M-]")  #'multi-vterm-next)
    (define-key vterm-copy-mode-map (kbd "C-q") #'vterm-copy-mode)

    ;; Substitute Misc Technical symbols that lack monospace glyphs in
    ;; Nerd Fonts.  Display table runs before font selection, so these
    ;; override the Unifont fontset fallback for these specific chars.
    ;; The glyphs here are used in Claude Code.
    (when buffer-display-table
      (aset buffer-display-table ?⏺ (vector ?●))   ; U+23FA → U+25CF
      (aset buffer-display-table ?⏵ (vector ?▶))   ; U+23F5 → U+25B6
      (aset buffer-display-table ?⏸ (vector ?‖)))  ; U+23F8 → U+2016

    ;; Suppress the cyan highlight Emacs draws on every U+00A0 (NBSP)
    ;; that Claude Code uses for banner padding.
    (setq-local nobreak-char-display nil))
  (add-hook 'vterm-mode-hook 'vterm-mode-custom-hook)

  ;; Strip fake newlines on entering copy mode so that
  ;; `toggle-truncate-lines' operates on real long lines instead of
  ;; the short segments vterm breaks them into for rendering.
  (setq vterm-copy-mode-remove-fake-newlines t)

  ;; Full-screen TUIs hide the cursor via DECTCEM (\e[?25l), which
  ;; vterm honors by setting buffer-local `cursor-type' to nil.
  ;; Copy mode inherits that invisible cursor.  Force a visible box
  ;; cursor on entry; exiting copy mode lets vterm re-sync it.
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (when vterm-copy-mode
                (setq-local cursor-type 'box))))

  ;; Pin monospace fallback fonts for Unicode blocks that TUI apps use
  ;; but Nerd Fonts lack — without this, Emacs falls back to
  ;; proportional fonts (STIX Two Math, Arial Unicode) whose taller
  ;; metrics cause vterm row jumps.  Using `font-spec' and replacing
  ;; (not prepending) prevents the CoreText fallback from overriding.
  (when *darwin*
    (set-fontset-font t '(#x2300 . #x23FF) (font-spec :family "Unifont")) ; Misc Technical
    (set-fontset-font t '(#x2700 . #x27BF) (font-spec :family "Menlo"))) ; Dingbats

  ;; libvterm lacks SGR 2 (faint/dim) support — it silently drops the
  ;; attribute.  Rewrite SGR 2 → SGR 90 (bright black) and SGR 22 →
  ;; SGR 22;39 (reset intensity + reset fg) on the raw byte stream
  ;; before it reaches the C module so faint text is visually distinct.
  ;; Codex uses faint/dim extensively.
  (defun +vterm-rewrite-faint (args)
    (let ((proc  (nth 0 args))
          (input (nth 1 args)))
      (if (and input (stringp input))
          (let ((s input))
            (setq s (replace-regexp-in-string "\033\\[2m"  "\033[90m"   s nil t))
            (setq s (replace-regexp-in-string "\033\\[22m" "\033[22;39m" s nil t))
            (list proc s))
        args)))
  (advice-add 'vterm--filter :filter-args #'+vterm-rewrite-faint)

  ;; --- Minibuffer anti-jump -------------------------------------------------
  ;; Two complementary fixes prevent vterm content from shifting when the
  ;; minibuffer activates or grows (e.g. Vertico candidates):
  ;;
  ;; 1. Suppress SIGWINCH: block `vterm--window-adjust-process-window-size'
  ;;    while the minibuffer is active so full-screen TUIs don't re-render
  ;;    for the temporarily smaller window.
  (defun +vterm-suppress-minibuffer-resize (orig-fn process windows)
    (unless (minibuffer-window-active-p (minibuffer-window))
      (funcall orig-fn process windows)))
  (advice-add 'vterm--window-adjust-process-window-size
              :around #'+vterm-suppress-minibuffer-resize)

  ;; 2. Pin scroll position: save and restore window-start/point for
  ;;    every vterm window so Emacs doesn't scroll to keep point visible
  ;;    when the window shrinks behind the minibuffer.
  (defun +vterm-save-window-starts ()
    (walk-windows
     (lambda (w)
       (when (with-current-buffer (window-buffer w)
               (derived-mode-p 'vterm-mode))
         (set-window-parameter w '+vterm-saved-start (window-start w))
         (set-window-parameter w '+vterm-saved-point (window-point w))))))

  (defun +vterm-pin-window-starts (_)
    (when (minibuffer-window-active-p (minibuffer-window))
      (walk-windows
       (lambda (w)
         (let ((saved-start (window-parameter w '+vterm-saved-start)))
           (when (and saved-start
                      (with-current-buffer (window-buffer w)
                        (derived-mode-p 'vterm-mode)))
             (set-window-start w saved-start)
             (set-window-point w saved-start)))))))

  (defun +vterm-clear-saved-starts ()
    (walk-windows
     (lambda (w)
       (when (window-parameter w '+vterm-saved-start)
         (let ((pt (window-parameter w '+vterm-saved-point)))
           (when pt (set-window-point w pt)))
         (set-window-parameter w '+vterm-saved-start nil)
         (set-window-parameter w '+vterm-saved-point nil)))
     nil t))

  (add-hook 'minibuffer-setup-hook    #'+vterm-save-window-starts)
  (add-hook 'minibuffer-exit-hook     #'+vterm-clear-saved-starts)
  (add-hook 'pre-redisplay-functions  #'+vterm-pin-window-starts))

(use-package multi-vterm
  :after vterm
  :commands (multi-vterm multi-vterm-dedicated-open multi-vterm-prev multi-vterm-next))

(provide 'init-multi-vterm)
