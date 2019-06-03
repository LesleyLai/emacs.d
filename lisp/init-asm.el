;; Assembly support
(use-package nasm-mode
  :ensure
  :config
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode)))

(provide 'init-asm)
