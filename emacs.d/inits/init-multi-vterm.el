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
    (define-key vterm-mode-map (kbd "S-<return>")
      (lambda () ; Shift+Return → insert newline instead of raw return (for TUI).
        (interactive)
        (vterm-send-string "\n")))
    ;; Exiting copy mode snaps point back to the process mark, collapsing
    ;; any active region — so a later M-w would only grab the cursor line.
    ;; Save the region to the kill-ring before leaving copy mode.
    ;;
    ;; Also rebind RET (shadowing the stock "vterm-copy-mode-done"), because
    ;; the stock version checks "use-region-p" — which requires the mark to
    ;; be *active*, not just set.  M-w (kill-ring-save) deactivates the mark
    ;; as part of its normal behavior, so the habit "M-w to copy, RET to
    ;; exit" silently falls back to a current-line copy: M-w's deactivation
    ;; makes RET think no region exists.  This version just copies an active
    ;; region (if any) and exits, never second-guessing with a line fallback.
    (defun +vterm-copy-save-and-exit ()
      (interactive)
      (when (region-active-p)
        (kill-ring-save (region-beginning) (region-end)))
      (vterm-copy-mode -1))
    (define-key vterm-copy-mode-map (kbd "C-q") #'+vterm-copy-save-and-exit)
    (define-key vterm-copy-mode-map (kbd "RET") #'+vterm-copy-save-and-exit)
    (define-key vterm-copy-mode-map [return]    #'+vterm-copy-save-and-exit)

    ;; Substitute specific Misc Technical, Geometric Shapes, etc symbols
    ;; used in TUI like Claude Code. Display table runs before font selection,
    ;; so these override the fontset fallback like Unifont and Menlo for these
    ;; specific chars.
    (when buffer-display-table
      (aset buffer-display-table ?⏺ (vector ?●))   ; U+23FA → U+25CF
      (aset buffer-display-table ?⏵ (vector ?▶))   ; U+23F5 → U+25B6
      (aset buffer-display-table ?⏸ (vector ?‖))   ; U+23F8 → U+2016
      (aset buffer-display-table ?◐ (vector ?◎)))   ; U+25D0 → U+25CE

    ;; --- TUI anti-nobreak-char ----------------------------------------------
    ;; Suppress the cyan highlight Emacs draws on every U+00A0 (NBSP)
    ;; that Claude Code uses for banner padding.
    (setq-local nobreak-char-display nil))

  ;; Override vterm bindings so the global directional window movement keys
  ;; still work inside vterm.
  (define-key vterm-mode-map (kbd "M-H") #'windmove-left)
  (define-key vterm-mode-map (kbd "M-J") #'windmove-down)
  (define-key vterm-mode-map (kbd "M-K") #'windmove-up)
  (define-key vterm-mode-map (kbd "M-L") #'windmove-right)

  (add-hook 'vterm-mode-hook 'vterm-mode-custom-hook)

  ;; --- TUI unicode fallback -------------------------------------------------
  ;; Pin monospace fallback fonts for Unicode blocks that TUI apps use
  ;; but Nerd Fonts lack — without this, Emacs falls back to proportional
  ;; fonts (STIX Two Math, Arial Unicode) whose taller metrics cause vterm
  ;; row jumps.  Using "font-spec" and replacing (not prepending) prevents
  ;; the CoreText fallback from overriding.
  ;; > brew install --cask font-gnu-unifont
  (when *darwin*
    (set-fontset-font t '(#x2300 . #x23FF) (font-spec :family "Unifont")) ; Misc Technical
    (set-fontset-font t '(#x25A0 . #x25FF) (font-spec :family "Menlo"))   ; Geometric Shapes
    (set-fontset-font t '(#x2700 . #x27BF) (font-spec :family "Menlo")))  ; Dingbats

  ;; --- TUI anti-cursor-hidden -----------------------------------------------
  ;; Full-screen TUIs hide the cursor via DECTCEM (\e[?25l), which
  ;; vterm honors by setting buffer-local "cursor-type" to nil.
  ;; Copy mode inherits that invisible cursor.  Force a visible box
  ;; cursor on entry; exiting copy mode lets vterm re-sync it.
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (when vterm-copy-mode
                (setq-local cursor-type 'box))))

  ;; --- TUI display faint/dim ------------------------------------------------
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

  ;; --- TUI repaint anti-flash -----------------------------------------------
  ;; Full-screen TUI apps (Claude Code, Codex) periodically repaint the
  ;; screen for minor UI changes (e.g. hiding a status hint).  The C
  ;; module's adjust_topline calls "recenter" after every redraw, which
  ;; can shift window-start and force Emacs into a full window redisplay
  ;; instead of an incremental line update — visible as a brief flash.
  ;; Pin window-start across redraws when the buffer line count is
  ;; unchanged (pure screen repaint, no new scrollback).
  ;;
  ;; (defun +vterm-stable-redraw (orig-fn buffer)
  ;;   (if (not (buffer-live-p buffer))
  ;;       (funcall orig-fn buffer)
  ;;     (let* ((old-nlines (with-current-buffer buffer
  ;;                          (count-lines (point-min) (point-max))))
  ;;            (saved (mapcar (lambda (w) (cons w (window-start w)))
  ;;                           (get-buffer-window-list buffer nil t))))
  ;;       (funcall orig-fn buffer)
  ;;       (when (and (buffer-live-p buffer)
  ;;                  (= old-nlines (with-current-buffer buffer
  ;;                                  (count-lines (point-min) (point-max)))))
  ;;         (dolist (entry saved)
  ;;           (when (window-live-p (car entry))
  ;;             (set-window-start (car entry) (cdr entry) t)))))))
  ;; (advice-add 'vterm--delayed-redraw :around #'+vterm-stable-redraw)

  ;; --- Minibuffer anti-jump -------------------------------------------------
  ;; Two complementary fixes prevent vterm content from shifting when the
  ;; minibuffer activates or grows (e.g. Vertico candidates):
  ;;
  ;; 1. Suppress SIGWINCH: block "vterm--window-adjust-process-window-size"
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
