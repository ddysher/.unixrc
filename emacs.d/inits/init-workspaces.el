;;------------------------------------------------------------------------------
;; Workspace management: window numbers via Winum and buffer-isolated
;; workspaces via Tabspaces.
;;------------------------------------------------------------------------------

(use-package winum
  :config
  ;; Reserve 0 for the minibuffer when it is active; other windows keep the
  ;; default sequential numbering.
  (setq winum-auto-assign-0-to-minibuffer t)

  ;; Enable the global minor mode so winum starts tracking windows and its
  ;; keymap becomes active.
  (winum-mode 1)

  ;; Use the winum minor-mode map so Meta-digit overrides Emacs's default
  ;; numeric prefix argument bindings.
  (define-key winum-keymap (kbd "M-0") #'winum-select-window-0)
  (define-key winum-keymap (kbd "M-1") #'winum-select-window-1)
  (define-key winum-keymap (kbd "M-2") #'winum-select-window-2)
  (define-key winum-keymap (kbd "M-3") #'winum-select-window-3)
  (define-key winum-keymap (kbd "M-4") #'winum-select-window-4)
  (define-key winum-keymap (kbd "M-5") #'winum-select-window-5)
  (define-key winum-keymap (kbd "M-6") #'winum-select-window-6)
  (define-key winum-keymap (kbd "M-7") #'winum-select-window-7)
  (define-key winum-keymap (kbd "M-8") #'winum-select-window-8)
  (define-key winum-keymap (kbd "M-9") #'winum-select-window-9))

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace
             tabspaces-switch-to-buffer
             tabspaces-switch-buffer-and-tab)
  :custom
  (tabspaces-keymap-prefix "C-c t")
  (tabspaces-default-tab "Default")
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-exclude-buffers '("*Compile-Log*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session nil)
  (tab-bar-show nil)                    ; do not show tabs at all
  (tab-bar-new-tab-choice "*scratch*")
  :config
  (tab-bar-mode 1)

  (with-eval-after-load 'consult
    (defvar consult-source-tabspace
      `(:name     "Workspace"
        :narrow   ?t
        :category tab
        :action   ,#'tabspaces-switch-or-create-workspace
        :items    ,#'tabspaces--list-tabspaces
        :annotate ,(lambda (workspace)
                     (when (string= workspace (tabspaces--current-tab-name))
                       " current")))
      "Tabspace source for `consult-buffer'.")

    (defvar consult-source-tabspace-buffer
      `(:name     "Workspace Buffer"
        :narrow   ?w
        :category buffer
        :face     consult-buffer
        :history  buffer-name-history
        :state    ,#'consult--buffer-state
        :items    ,(lambda ()
                     (consult--buffer-query
                      :sort 'visibility
                      :as #'consult--buffer-pair
                      :predicate #'tabspaces--local-buffer-p)))
      "Workspace-local buffer source for `consult-buffer'.")

    (defun laura/consult-source-put (source property value)
      "Set PROPERTY to VALUE on Consult SOURCE."
      (setf (plist-get (symbol-value source) property) value))

    (defun laura/consult-tabspaces-setup ()
      "Prefer workspace-local buffers in `consult-buffer' while Tabspaces is on."
      (cond
       (tabspaces-mode
        (laura/consult-source-put 'consult-source-buffer :hidden t)
        (laura/consult-source-put 'consult-source-buffer :default nil)
        (add-to-list 'consult-buffer-sources 'consult-source-tabspace)
        (add-to-list 'consult-buffer-sources 'consult-source-tabspace-buffer))
       (t
        (laura/consult-source-put 'consult-source-buffer :hidden nil)
        (laura/consult-source-put 'consult-source-buffer :default t)
        (setq consult-buffer-sources
              (remove 'consult-source-tabspace-buffer
                      (remove 'consult-source-tabspace consult-buffer-sources))))))

    (add-hook 'tabspaces-mode-hook #'laura/consult-tabspaces-setup)
    (laura/consult-tabspaces-setup)))

(provide 'init-workspaces)
