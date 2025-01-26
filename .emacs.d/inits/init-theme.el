;;------------------------------------------------------------------------------
;; Load theme and font
;;------------------------------------------------------------------------------
(require-package 'zenburn-theme)
(require-package 'material-theme)

(defun load-zenburn ()
  (load-theme 'zenburn t)
  ;; Make active buffer more highlighted, used with zenburn. Color code borrowed
  ;; from zenburn-theme.el, so this need to be work with zenburn.
  (set-face-attribute 'mode-line nil
                      :foreground "gray80" :background "IndianRed4"
                      :box '(:line-width 1 :style released-button))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground "#5F7F5F" :background "#383838"
                      :box '(:line-width 1 :style released-button)))

(defun load-material ()
  (load-theme `material t))

;; Load commonly used themes, options are listed below:
;;   (load-zenburn)
;;   (load-material)
(load-material)

;; Load fonts, options are:
(cond (*linux*  (set-frame-font "Hack-9.5")))
;; (cond (*linux*  (set-frame-font "Source Code Pro")))
;; (cond (*linux*  (set-frame-font "DejaVu Sans Mono-10")))

(cond (*darwin* (set-frame-font "Monaco-11")))

;; Machine specifc fonts
;; (cond (*macair* (set-frame-font "Monaco-10")))

(provide 'init-theme)
