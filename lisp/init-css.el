;; Colourise  colour literals
;; web-mode does not like rainbow-mode
(use-package rainbow-mode
             :ensure t
             :config
             (add-hook 'css-mode-hook 'rainbow-mode))

(provide 'init-css)
