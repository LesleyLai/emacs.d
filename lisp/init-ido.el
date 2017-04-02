(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode t)
  (ido-everywhere 1))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode 1)) ;; Use ido everywhere

;; smex for M-x enhancement
(use-package smex
             :ensure t
             :config
             (smex-initialize)
             (global-set-key (kbd "M-x") 'smex)
             (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(provide 'init-ido)
