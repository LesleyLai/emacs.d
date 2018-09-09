(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(customize-set-variable 'package-archive-priorities '(("melpa" . 1)))

;; Adopts Use package to on-demand installation of packages
;; {{
(defvar my-packages '(use-package
                      use-package-chords
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(require 'use-package)
(require 'bind-key)                ;; if you use any :bind variant
;; }}

(use-package counsel :ensure t) ;; auto completion tool

(provide 'init-elpa)
