(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

;; Adopts Use package to on-demand installation of packages
;; {{
(defvar my-packages '(use-package
                      use-package-chords
                      ))

(package-initialize)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(require 'use-package)
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant
;; }}

(use-package counsel :ensure t) ;; auto completion tool
(use-package magit :ensure t) ;; ui for git
(use-package yasnippet :ensure t)


(provide 'init-elpa)
