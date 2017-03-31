(require-package 'company)
(require-package 'company-c-headers)
(require-package 'company-tern)

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-c-headers)
     (add-to-list 'company-backends 'company-tern)))

(provide 'init-company)
