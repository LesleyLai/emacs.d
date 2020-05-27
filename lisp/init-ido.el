(use-package ido
  :ensure t
  :custom
  (ido-create-new-buffer 'always)
  :config
  (customize-set-variable 'ido-enable-flex-matching t)
  (customize-set-variable 'ido-default-file-method 'selected-window)
  (customize-set-variable 'ido-auto-merge-work-directories-length -1)
  (ido-mode t)
  (ido-everywhere 1))
  )

(use-package ido-completing-read+
  :ensure t
  :defer 1
  :config
  (ido-ubiquitous-mode 1)) ;; Use ido everywhere

(use-package flx-ido
  :ensure t
  :defer 1
  :config
  (flx-ido-mode +1)
  ;; disable ido faces to see flx highlights
  (customize-set-variable 'ido-use-faces nil)
  )

;; amx for M-x enhancement
(use-package amx
  :ensure t
  :after (:any ivy ido)
  :config (amx-mode))

(provide 'init-ido)
