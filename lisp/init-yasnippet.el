(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :defer t
  :init
  (yas-reload-all)
  
  (defun yasnippet-generic-setup-for-mode-hook ()
    (yas-minor-mode 1))

  (add-hook 'prog-mode-hook 'yasnippet-generic-setup-for-mode-hook)
  (add-hook 'text-mode-hook 'yasnippet-generic-setup-for-mode-hook)
  ;; below modes does NOT inherit from prog-mode
  (add-hook 'cmake-mode-hook 'yasnippet-generic-setup-for-mode-hook)
  (add-hook 'web-mode-hook 'yasnippet-generic-setup-for-mode-hook)
  (add-hook 'scss-mode-hook 'yasnippet-generic-setup-for-mode-hook)
  )

(provide 'init-yasnippet)
