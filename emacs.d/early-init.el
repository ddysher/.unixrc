;;------------------------------------------------------------------------------
;; Early init - runs before init.el and package.el initialization
;;------------------------------------------------------------------------------

;; Raise GC threshold during startup to reduce GC pauses, restore after.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024)))) ; 16MB

;; Prevent package.el from loading packages before init.el runs.
;; We call (package-initialize) explicitly in init-elpa.el.
(setq package-enable-at-startup nil)

;; Use package-quickstart to load a single cached autoloads file instead
;; of scanning every elpa package directory at startup.
(setq package-quickstart t)
(setq package-quickstart-file
      (expand-file-name "auto-managed/package-quickstart.el" user-emacs-directory))

;; Redirect native-compiled .eln files out of the emacs.d root.
(when (featurep 'native-compile)
  (startup-redirect-eln-cache
   (expand-file-name "auto-managed/eln-cache" user-emacs-directory)))

;; Disable UI elements early to avoid momentary display.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Avoid resizing frame during startup (font/theme changes).
(setq frame-inhibit-implied-resize t)
