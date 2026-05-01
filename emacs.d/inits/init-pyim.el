;;------------------------------------------------------------------------------
;; Provide pyim, an elegant chinese input method:
;;   https://github.com/tumashu/pyim
;;
;; Common commands:
;;   M-x pyim-dicts-manager - Manage all dictionaries.
;;   M-x pyim-restart - Restart pyim.
;;
;; Notes:
;; 1. Dictionary 'pyim-bigdict' is ~20M so it is ignored; to install:
;;    wget http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim -P ~/.emacs.d/etc/pyim/dicts
;; 2. Only enabled on Linux, use "C-;" to toggle input method.
;;------------------------------------------------------------------------------

(when *linux*
  (use-package pyim-basedict
    :demand t
    :config
    (pyim-basedict-enable))

  (use-package pyim
    :demand t
    :after pyim-basedict
    :bind ("C-'" . toggle-input-method)
    :config
    (setq default-input-method "pyim")
    (setq pyim-dicts
          '((:name "pyim-bigdict"
             :file "~/.emacs.d/etc/pyim/dicts/pyim-bigdict.pyim"
             :coding utf-8-unix
             :dict-type pinyin-dict)))))

(provide 'init-pyim)
