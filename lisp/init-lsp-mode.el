(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp
  :init
  (setq lsp-auto-guess-root nil)
  (setq lsp-prefer-flymake nil)
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (css-mode . lsp)
         (python-mode . lsp)
         (html-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp)
         (js2-mode . lsp)
         (rust-mode . lsp))
  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'lsp-after-open-hook #'lsp-ui-mode)
  (setq lsp-ui-doc-enable t
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
  (setq company-lsp-enable-recompletion 'auto
        company-lsp-enable-snippet t
        company-lsp-filter-candidates nil
        company-lsp-cache-candidates t
        company-lsp-async t)
  )


(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  )

(provide 'init-lsp-mode)
