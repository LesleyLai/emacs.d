(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'lsp-after-open-hook #'lsp-ui-mode)
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        ;; lsp-ui-doc-position 'at-point
        )
  )

(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends)
  :config
  (setq company-lsp-enable-recompletion t
        company-lsp-enable-snippet t
        company-lsp-cache-candidates t
        company-lsp-async t)
  )

(use-package lsp-treemacs
  :ensure t
  )

(provide 'init-lsp-mode)
