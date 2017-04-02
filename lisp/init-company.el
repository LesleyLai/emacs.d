(use-package company
             :ensure t
             :config
             (add-hook 'after-init-hook 'global-company-mode))

(use-package company-c-headers
             :ensure t)

(use-package company-tern
             :ensure t)

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-c-headers)
     (add-to-list 'company-backends 'company-tern)))

(provide 'init-company)
