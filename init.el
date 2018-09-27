
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar emacs-load-start-time (current-time))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;--------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;--------------------------------------------------------------------
(defvar *is-a-mac* (eq system-type 'darwin))
(defvar *win64* (eq system-type 'windows-nt) )
(defvar *cygwin* (eq system-type 'cygwin) )
(defvar *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(defvar *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(defvar *use-GUI* (display-graphic-p))

;;-------------------------------------------------------------------
;; Bootstrap config
;; Do those before other things
;;-------------------------------------------------------------------
(require 'init-elpa)
(require 'init-elisp)

;; use package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-verbose t)
(require 'use-package)

;; Always compile packages, and use the newest version available.
(use-package auto-compile
  :ensure
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)


;; Customize {{
;; Theme
(setq custom-theme-directory "~/.emacs.d/themes")

;; Private Setting
(defconst custom-file "~/.emacs.d/custom/custom.el")
(if (file-exists-p custom-file) (load-file custom-file))
;; }}

;;-------------------------------------------------------------------
;; Components
;;-------------------------------------------------------------------
(require 'init-keybinding)
(require 'init-company)
(require 'init-csv)
(require 'init-css)
(require 'init-cc-mode)
(require 'init-dired)
(require 'init-flycheck)
(require 'init-python)
(require 'init-ido)
(require 'init-lisp)
(require 'init-lsp-mode)
(require 'init-ui)
(require 'init-javascript)
(require 'init-markdown)
(require 'init-misc)
(require 'init-web)
(require 'init-ruby)
(require 'init-racket)
(require 'init-rust)
(require 'init-scala)
(require 'init-org)
(require 'init-sh)
(require 'init-glsl-mode)
(require 'init-yasnippet)
(require 'init-yaml)
(require 'init-frame-hooks)
(require 'init-speedbar)
(require 'init-writing)
(require 'init-typescript)
(require 'init-elm)
(require 'init-latex)

;;-------------------------------------------------------------------
;; Lazy loading components that do not need at start up
;;-------------------------------------------------------------------
(use-package idle-require
  :ensure t
  :config
  (setq idle-require-idle-delay 1)
  (setq idle-require-symbols '(init-misc-lazy
                               init-git
                               init-multiple-cursors
                               init-ibuffer
                               init-spelling
                               ))
  (idle-require-mode 1) ;; starts loading
  )


(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds."
           (time-to-seconds (time-since emacs-load-start-time))))
