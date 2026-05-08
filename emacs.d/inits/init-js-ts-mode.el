;;------------------------------------------------------------------------------
;; JavaScript and TypeScript modes.
;;
;; Uses Emacs 30 built-in tree-sitter modes for:
;;   JavaScript: .js, .mjs, .cjs, .es6, .jsx
;;   TypeScript: .ts, .mts, .cts, .tsx
;;   Styles:     .css, .scss
;;   Markup:     .html, .htm
;;   Config:     .json, .jsonc, .babelrc, .eslintrc, .prettierrc, tsconfig*.json
;;
;; Dependencies:
;;   npm install -g typescript-language-server typescript
;;
;; Tree-sitter grammars:
;;   javascript, typescript, tsx, css, html, json
;; Install once with: M-x treesit-install-language-grammar
;; Grammars are loaded from var/treesit via no-littering.
;;------------------------------------------------------------------------------

;; Remap legacy modes to tree-sitter powered modes.
(dolist (remap '((javascript-mode . js-ts-mode)
                 (js-mode . js-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (css-mode . css-ts-mode)
                 (html-mode . html-ts-mode)
                 (json-mode . json-ts-mode)))
  (add-to-list 'major-mode-remap-alist remap))

(use-package js
  :ensure nil
  :mode (("\\.m?js\\'" . js-ts-mode)
         ("\\.cjs\\'"  . js-ts-mode)
         ("\\.es6\\'"  . js-ts-mode)
         ("\\.jsx\\'"  . js-ts-mode))
  :interpreter ("node" . js-ts-mode))

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.m?ts\\'" . typescript-ts-mode)
         ("\\.cts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'"  . tsx-ts-mode)))

(use-package css-mode
  :ensure nil
  :mode (("\\.css\\'"  . css-ts-mode)
         ("\\.scss\\'" . css-ts-mode)))

(use-package html-mode
  :ensure nil
  :mode ("\\.html?\\'" . html-ts-mode))

(use-package json-ts-mode
  :ensure nil
  :mode (("\\.json\\'"          . json-ts-mode)
         ("\\.jsonc\\'"         . json-ts-mode)
         ("\\.babelrc\\'"       . json-ts-mode)
         ("\\.eslintrc\\'"      . json-ts-mode)
         ("\\.prettierrc\\'"    . json-ts-mode)
         ("tsconfig.*\\.json\\'" . json-ts-mode)))

;; Register typescript-language-server for JavaScript and TypeScript modes.
;; Eglot base config lives in init-completion.el.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-ts-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio"))))

;; JS/TS modes derive from prog-mode, so prog-mode-custom-hook enables shared
;; editor behavior such as tempel-abbrev-mode.
(dolist (hook '(js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook #'eglot-ensure))

(provide 'init-js-ts-mode)
