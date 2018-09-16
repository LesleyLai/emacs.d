(use-package rust-mode
  :ensure t
  :config
  (customize-set-variable 'rust-format-on-save t))

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

(use-package lsp-rust
  :ensure t
  :disabled t
  :after lsp-mode
  :init
  (add-hook 'rust-mode-hook #'lsp-rust-enable))

(provide 'init-rust)
