;;------------------------------------------------------------------------------
;; Provide web mode, a major mode for editing html template.
;;------------------------------------------------------------------------------
(require-package 'web-mode)
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode)) ;; ERB
(add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode)) ;; Plain HTML
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode)) ;; JS + JSX
(add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode)) ;; ES6
(add-to-list 'auto-mode-alist '("\\.css\\'"    . web-mode)) ;; CSS
(add-to-list 'auto-mode-alist '("\\.scss\\'"   . web-mode)) ;; SCSS
(add-to-list 'auto-mode-alist '("\\.php\\'"    . web-mode)) ;; PHP
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode)) ;; Blade template

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
;; Customize indentations (indentation inside code).
(setq web-mode-markup-indent-offset 2) ; HTML
(setq web-mode-css-indent-offset 2)    ; CSS
(setq web-mode-code-indent-offset 2)   ; Script
;; Customize padding (indentation inside tags).
(setq web-mode-style-padding 2)       ; For <style> parts
(setq web-mode-script-padding 2)      ; for <script> parts


;; disable jshint since we prefer eslint checking
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode))

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
  (auto-complete-mode))

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
