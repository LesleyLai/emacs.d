(setq package-user-dir "~/.emacs.d/elpa"
      package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa stable" . "http://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")))

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Adopts Use package to on-demand installation of packages
;; {{
;; Install use-package if not installed yet.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'use-package-chord))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))
;; }}

(use-package auto-package-update
  :ensure t
  :defer 5
  :config
  (customize-set-variable 'auto-package-update-interval 7) ;; in days
  (customize-set-variable 'auto-package-update-prompt-before-update t)
  (customize-set-variable 'auto-package-update-delete-old-versions t)
  (customize-set-variable 'auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package counsel :ensure t :defer 1) ;; auto completion tool

(provide 'init-elpa)
