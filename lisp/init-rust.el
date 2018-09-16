(use-package lsp-rust
  :ensure t
  :after lsp-mode
  :init
  (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls")))

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . (lambda ()
                        (lsp-rust-enable)
                        (lsp-ui-mode)
                        (lsp-ui-sideline-mode)
                        (lsp-ui-doc-mode)
                        (eldoc-mode -1)
                        (flycheck-mode)
                        (smart-dash-mode))))
  :config
  (customize-set-variable 'rust-format-on-save t)
  )

(use-package cargo
  :ensure t
  :after (rust-mode)
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-r") #'cargo-process-run)
              (local-set-key (kbd "C-c C-t") #'cargo-process-test)
              )))

(use-package flycheck-rust
  :ensure t
  :after (rust-mode)
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))



(provide 'init-rust)
