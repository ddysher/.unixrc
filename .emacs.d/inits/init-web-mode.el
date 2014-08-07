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
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; Enable autocomplete minor mode in web-mode.
(setq web-mode-ac-sources-alist
      '(("javascript" . (ac-source-yasnippet
                         ac-source-abbrev
                         ac-source-dictionary
                         ac-source-words-in-same-mode-buffers))
        ("html" . (ac-source-yasnippet
                   ac-source-abbrev
                   ac-source-dictionary
                   ac-source-words-in-same-mode-buffers))
        ("css" . (ac-source-yasnippet
                  ac-source-css-property
                  ac-source-abbrev
                  ac-source-dictionary
                  ac-source-words-in-same-mode-buffers))))

(defun web-mode-custom-hook ()
  ;; Need to enable AC/yas here.
  (yas-minor-mode)
  (auto-complete-mode)
  ;; Customize indentations (indentation inside code).
  (setq web-mode-markup-indent-offset universal-indent-size) ; HTML
  (setq web-mode-css-indent-offset universal-indent-size)    ; CSS
  (setq web-mode-code-indent-offset universal-indent-size)   ; Script
  ;; Customize padding (indentation inside tags).
  (setq web-mode-style-padding 2)       ; For <style> parts
  (setq web-mode-script-padding 2))     ; for <script> parts

(defun web-mode-before-auto-complete-custom-hooks ()
    (let ((web-mode-cur-language
           (web-mode-language-at-pos)))
      (if (string= web-mode-cur-language "javascript")
          (yas-activate-extra-mode 'js-mode)
        (yas-deactivate-extra-mode 'js-mode))
      (if (string= web-mode-cur-language "html")
          (yas-activate-extra-mode 'html-mode)
        (yas-deactivate-extra-mode 'html-mode))
      (if (string= web-mode-cur-language "css")
          (yas-activate-extra-mode 'css-mode)
        (yas-deactivate-extra-mode 'css-mode))))

(add-hook 'web-mode-hook 'web-mode-custom-hook)
(add-hook 'web-mode-before-auto-complete-hooks
          'web-mode-before-auto-complete-custom-hooks)


(provide 'init-web-mode)
