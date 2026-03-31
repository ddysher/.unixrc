;;------------------------------------------------------------------------------
;; Provide web mode, a major mode for editing web related files.
;;------------------------------------------------------------------------------
(require-package 'web-mode)

;; Associate theses files with web-mode (works before web-mode is loaded).
(add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))

(with-eval-after-load 'web-mode
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")
          ("javascript" . "\\.es6?\\'")))

  (setq web-mode-engines-alist
        '(("blade"  . "\\.blade\\.")))

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "js")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2))

(defun web-mode-custom-hook ()
  (yas-minor-mode))

(defun web-mode-before-auto-complete-custom-hooks ()
  (let ((web-mode-cur-language
         (web-mode-language-at-pos)))
    (if (or (string= web-mode-cur-language "javascript") (string= web-mode-cur-language "jsx"))
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
