;; flycheck for syntax check
(use-package flycheck
             :ensure t
             :config
             (add-hook 'prog-mode-hook 'global-flycheck-mode)
             (add-hook 'css-mode-hook 'global-flycheck-mode))

(use-package flycheck-cstyle
             :ensure t
             :after flycheck
             :config
             (progn
               (require 'flycheck-cstyle)
               (flycheck-cstyle-setup)
               ;; chain after cppcheck since this is the last checker in the upstream
               ;; configuration
               (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))))

(provide 'init-flycheck)
