;; Colourise  colour literals
;; web-mode does not like rainbow-mode
(use-package rainbow-mode
  :ensure t
  :defer t
  :config
  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

(provide 'init-css)
