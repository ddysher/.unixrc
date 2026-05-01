;;------------------------------------------------------------------------------
;; Web development modes: js2-mode and web-mode.
;;
;; js2-mode: JavaScript major mode with syntax checking; handles .js and .jsx.
;; web-mode: major mode for HTML templates, CSS, and other web file types.
;;------------------------------------------------------------------------------

;; JavaScript and JSX (js2-mode).
(use-package js2-mode
  :mode (("\\.js\\'"  . js2-mode)
         ("\\.jsx\\'" . js2-mode))
  :interpreter ("node" . js2-mode)
  :hook ((js-mode . (lambda () (setq js-indent-level default-indent-size)))
         (js2-mode . (lambda ()
                       (setq js2-basic-offset default-indent-size)
                       (setq js2-strict-inconsistent-return-warning nil)))))

(defun web-mode-custom-hook ()
  (tempel-abbrev-mode))

;; Web templates and frontend files (web-mode).
(use-package web-mode
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.html?\\'"     . web-mode)
         ("\\.es6\\'"       . web-mode)
         ("\\.css\\'"       . web-mode)
         ("\\.scss\\'"      . web-mode)
         ("\\.php\\'"       . web-mode)
         ("\\.blade\\.php\\'" . web-mode))
  :hook (web-mode . web-mode-custom-hook)
  :config
  (setq web-mode-content-types-alist
        '(("javascript" . "\\.es6?\\'")))

  (setq web-mode-engines-alist
        '(("blade"  . "\\.blade\\.")))

  (define-advice web-mode-highlight-part (:around (orig-fn &rest args) tweak-jsx)
    (if (member web-mode-content-type '("jsx" "js"))
        (let ((web-mode-enable-part-face nil))
          (apply orig-fn args))
      (apply orig-fn args)))

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2))

(provide 'init-web-mode)
