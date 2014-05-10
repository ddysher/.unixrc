;;------------------------------------------------------------------------------
;; Provide multi-web mode, a elpa managed package.  It will choose appropriate
;; major mode.
;;------------------------------------------------------------------------------
(require-package 'multi-web-mode)
(require 'multi-web-mode)


(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)


(provide 'init-multi-web)
