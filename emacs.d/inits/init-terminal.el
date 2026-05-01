;;------------------------------------------------------------------------------
;; terminal-mode: enhanced terminal emulation with workarounds for full-screen.
;; The mode sets up ghostty and vterm.
;; - ghostel is a emacs terminal based on libghostty
;; - vterm is a emacs terminal based on libvterm
;; - eat is a terminal emulator alternative to builtin term
;;------------------------------------------------------------------------------

(require 'cl-lib)

;;------------------------------------------------------------------------------
;; ghostel, used primarily for TUI
;;------------------------------------------------------------------------------
(use-package ghostel
  :defer t
  :config
  (defvar +ghostel-buffer-list nil
    "Ghostel buffers in creation order.")

  (defun +ghostel-track-buffer ()
    (unless (memq (current-buffer) +ghostel-buffer-list)
      (setq +ghostel-buffer-list
            (nconc +ghostel-buffer-list (list (current-buffer))))))

  (defun +ghostel-forget-buffer ()
    (setq +ghostel-buffer-list
          (delq (current-buffer) +ghostel-buffer-list)))

  (defun +ghostel-switch (direction &optional offset)
    (setq +ghostel-buffer-list
          (cl-remove-if-not
           (lambda (buffer)
             (and (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (derived-mode-p 'ghostel-mode))))
           +ghostel-buffer-list))
    (let* ((buffers +ghostel-buffer-list)
           (count (length buffers))
           (index (cl-position (current-buffer) buffers))
           (step (or offset 1)))
      (cond
       ((zerop count) (ghostel))
       (index
        (switch-to-buffer
         (nth (mod (if (eq direction 'next)
                       (+ index step)
                     (- index step))
                   count)
              buffers)))
       (t (switch-to-buffer (car buffers))))))

  (defun +ghostel-next (&optional offset)
    "Switch to the next Ghostel buffer."
    (interactive "p")
    (+ghostel-switch 'next offset))

  (defun +ghostel-prev (&optional offset)
    "Switch to the previous Ghostel buffer."
    (interactive "p")
    (+ghostel-switch 'prev offset))

  (define-key ghostel-mode-map [f1]  #'find-file)
  (define-key ghostel-mode-map [f2]  #'save-buffer)
  (define-key ghostel-mode-map [f3]  #'kill-current-buffer)
  (define-key ghostel-mode-map [f4]  #'revert-buffer)
  (define-key ghostel-mode-map [f5]  #'tabspaces-open-or-create-project-and-workspace)
  (define-key ghostel-mode-map [f6]  #'tabspaces-save-session)
  (define-key ghostel-mode-map [f7]  #'laura/tabspaces-kill-workspace)
  (define-key ghostel-mode-map [f8]  #'tab-list)
  (define-key ghostel-mode-map [f9]  #'dirvish)
  (define-key ghostel-mode-map [f10] #'consult-ripgrep)
  (define-key ghostel-mode-map [f12] #'multi-vterm)
  (define-key ghostel-mode-map (kbd "C-o")  #'consult-buffer)
  (define-key ghostel-mode-map (kbd "C-q")  #'ghostel-copy-mode)
  (define-key ghostel-mode-map (kbd "M-[")  #'+ghostel-prev)
  (define-key ghostel-mode-map (kbd "M-]")  #'+ghostel-next)
  (define-key ghostel-copy-mode-map (kbd "C-q") #'ghostel-copy-mode)
  (define-key ghostel-copy-mode-map (kbd "M-[") #'+ghostel-prev)
  (define-key ghostel-copy-mode-map (kbd "M-]") #'+ghostel-next)

  (defun ghostel-mode-custom-hook ()
    (+ghostel-track-buffer)
    (setq-local nobreak-char-display nil)
    (add-hook 'kill-buffer-hook #'+ghostel-forget-buffer nil t))
  (add-hook 'ghostel-mode-hook #'ghostel-mode-custom-hook))

;;------------------------------------------------------------------------------
;; vterm, for regular terminal usage.
;;------------------------------------------------------------------------------
(use-package vterm
  :defer t
  :init
  (blink-cursor-mode -1)
  :config
  ;; Keymap bindings — set once on package load, not per-buffer.
  (define-key vterm-mode-map [f1]  #'find-file)
  (define-key vterm-mode-map [f2]  #'save-buffer)
  (define-key vterm-mode-map [f3]  #'kill-current-buffer)
  (define-key vterm-mode-map [f4]  #'revert-buffer)
  (define-key vterm-mode-map [f5]  #'tabspaces-open-or-create-project-and-workspace)
  (define-key vterm-mode-map [f6]  #'tabspaces-save-session)
  (define-key vterm-mode-map [f7]  #'laura/tabspaces-kill-workspace)
  (define-key vterm-mode-map [f8]  #'tab-list)
  (define-key vterm-mode-map [f9]  #'dirvish)
  (define-key vterm-mode-map [f10] #'consult-ripgrep)
  (define-key vterm-mode-map [f12] #'multi-vterm)
  (define-key vterm-mode-map (kbd "C-o")  #'consult-buffer)
  (define-key vterm-mode-map (kbd "C-q")  #'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "M-[")  #'multi-vterm-prev)
  (define-key vterm-mode-map (kbd "M-]")  #'multi-vterm-next)
  (define-key vterm-mode-map (kbd "S-<return>")
    (lambda () ; Shift+Return → insert newline instead of raw return (for TUI).
      (interactive)
      (vterm-send-string "\n")))
  (define-key vterm-mode-map (kbd "M-H") #'windmove-left)
  (define-key vterm-mode-map (kbd "M-J") #'windmove-down)
  (define-key vterm-mode-map (kbd "M-K") #'windmove-up)
  (define-key vterm-mode-map (kbd "M-L") #'windmove-right)

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

  ;; Per-buffer hook — only buffer-local setup belongs here.
  (defvar +vterm-global-hooks-enabled nil
    "Non-nil when the global vterm minibuffer hooks are installed.")

  (defun +vterm-enable-global-hooks ()
    (unless +vterm-global-hooks-enabled
      (add-hook 'minibuffer-setup-hook   #'+vterm-save-window-starts)
      (add-hook 'minibuffer-exit-hook    #'+vterm-clear-saved-starts)
      (add-hook 'pre-redisplay-functions #'+vterm-pin-window-starts)
      (setq +vterm-global-hooks-enabled t)))

  (defun +vterm-disable-global-hooks-if-unused ()
    (unless (cl-some (lambda (buffer)
                       (and (not (eq buffer (current-buffer)))
                            (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (derived-mode-p 'vterm-mode))))
                     (buffer-list))
      (remove-hook 'minibuffer-setup-hook   #'+vterm-save-window-starts)
      (remove-hook 'minibuffer-exit-hook    #'+vterm-clear-saved-starts)
      (remove-hook 'pre-redisplay-functions #'+vterm-pin-window-starts)
      (setq +vterm-global-hooks-enabled nil)))

  (defun vterm-mode-custom-hook ()
    ;; --- TUI anti-nobreak-char ----------------------------------------------
    ;; Suppress the cyan highlight Emacs draws on every U+00A0 (NBSP)
    ;; that Claude Code uses for banner padding.
    (setq-local nobreak-char-display nil)
    (+vterm-enable-global-hooks)
    (add-hook 'kill-buffer-hook #'+vterm-disable-global-hooks-if-unused nil t))

  (add-hook 'vterm-mode-hook #'vterm-mode-custom-hook)

  ;; --- TUI anti-cursor-hidden -----------------------------------------------
  ;; Full-screen TUIs hide the cursor via DECTCEM (\e[?25l), which vterm
  ;; honors by setting "cursor-type" to nil.  Copy mode inherits that
  ;; invisible cursor, so force a box cursor on entry.  On exit, restore
  ;; the prior value — vterm's "cursor_type_changed" flag is one-shot and
  ;; libvterm won't re-emit the change, so without explicit restore the
  ;; cursor would stay visible in terminal mode too.  That's the Cursor CLI
  ;; bug: agent emits "\e[?25l" once at startup and never again; if copy
  ;; mode ran even once, cursor-type stuck at box forever.
  (defvar-local +vterm-pre-copy-cursor-type nil)
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (if vterm-copy-mode
                  (progn
                    (setq +vterm-pre-copy-cursor-type cursor-type)
                    (setq-local cursor-type 'box))
                (setq-local cursor-type +vterm-pre-copy-cursor-type))
              (force-mode-line-update)))

  ;; --- TUI display faint/dim + altscreen cursor hide ------------------------
  ;; Two rewrites on the raw byte stream before it reaches the C module:
  ;;
  ;; 1. libvterm lacks SGR 2 (faint/dim) support — it silently drops the
  ;;    attribute.  Rewrite SGR 2 → SGR 90 (bright black) and SGR 22 →
  ;;    SGR 22;39 (reset intensity + reset fg) so faint text is visually
  ;;    distinct.  Codex uses faint/dim extensively.
  ;;
  ;; 2. Some TUIs (Cursor CLI) enter the alternate screen buffer without
  ;;    sending DECTCEM hide (\e[?25l), so vterm leaves "cursor-type"
  ;;    at its default and emacs draws a stray cursor at point — typically
  ;;    stuck at the bottom of the buffer since the TUI repaints absolutely.
  ;;    Inject DECTCEM hide after every altscreen-enter (\e[?1049h,
  ;;    \e[?47h) and DECTCEM show before every altscreen-exit (\e[?1049l,
  ;;    \e[?47l).  Claude/Codex already send their own hide; the extra
  ;;    sequence is a no-op for them.
  (defun +vterm-filter-rewrite (args)
    (let ((proc  (nth 0 args))
          (input (nth 1 args)))
      (if (and input (stringp input))
          (let ((s input))
            (setq s (replace-regexp-in-string "\033\\[2m"  "\033[90m"    s nil t))
            (setq s (replace-regexp-in-string "\033\\[22m" "\033[22;39m" s nil t))
            (setq s (replace-regexp-in-string "\033\\[\\?1049h" "\033[?1049h\033[?25l" s nil t))
            (setq s (replace-regexp-in-string "\033\\[\\?47h"   "\033[?47h\033[?25l"   s nil t))
            (setq s (replace-regexp-in-string "\033\\[\\?1049l" "\033[?25h\033[?1049l" s nil t))
            (setq s (replace-regexp-in-string "\033\\[\\?47l"   "\033[?25h\033[?47l"   s nil t))
            (list proc s))
        args)))
  (advice-add 'vterm--filter :filter-args #'+vterm-filter-rewrite)

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

  ;; These hooks are installed lazily while vterm buffers exist,
  ;; then removed again once the last vterm buffer is killed.
  )

;;------------------------------------------------------------------------------
;; eat, packages dependencies, not for personal usage.
;;------------------------------------------------------------------------------
(use-package eat
  :defer t)

;;------------------------------------------------------------------------------
;; multi-vterm, supports opening and switching multiple vterm quickly.
;;------------------------------------------------------------------------------
(use-package multi-vterm
  :after vterm
  :commands (multi-vterm multi-vterm-dedicated-open multi-vterm-prev multi-vterm-next))

(provide 'init-terminal)
