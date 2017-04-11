(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init
  (yas-reload-all)
  
  ;; my private snippets, should be placed before enabling yasnippet
  (setq my-yasnippets (expand-file-name "~/my-yasnippets"))
  (if (and  (file-exists-p my-yasnippets) (not (member my-yasnippets yas-snippet-dirs)))
      (add-to-list 'yas-snippet-dirs my-yasnippets))

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
