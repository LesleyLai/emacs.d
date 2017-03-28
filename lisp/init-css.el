;; Colourise CSS colour literals
;; web-mode does not like rainbow-mode
(dolist (hook '(css-mode-hook))
  (add-hook hook 'rainbow-mode))

(provide 'init-css)
