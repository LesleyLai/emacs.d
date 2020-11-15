(use-package glsl-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.tesc\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.tese\\'" . glsl-mode)))

(provide 'init-glsl-mode)
