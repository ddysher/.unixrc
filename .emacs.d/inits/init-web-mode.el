;;------------------------------------------------------------------------------
;; Provide web mode, a major mode for editing html template.
;;------------------------------------------------------------------------------
(require-package 'web-mode)
(require 'web-mode)


;; Associate theses files with web-mode.
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(defun web-mode-custom-hook ()
  ;; Customize indentations (indentation inside code).
  (setq web-mode-markup-indent-offset universal-indent-size) ; HTML
  (setq web-mode-css-indent-offset universal-indent-size)    ; CSS
  (setq web-mode-code-indent-offset universal-indent-size)   ; Script
  ;; Customize padding.
  (setq web-mode-style-padding 2)       ; For <style> parts
  (setq web-mode-script-padding 2))     ; for <script> parts

(add-hook 'web-mode-hook 'web-mode-custom-hook)


(provide 'init-web-mode)
