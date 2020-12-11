(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :config
  (yas-reload-all)
  :diminish yas-minor-mode

  :hook ((prog-mode
          text-mode
          cmake-mode
          web-mode
          scss-mode
          latex-mode)
         . (lambda () (yas-minor-mode 1)))
  )

(provide 'init-yasnippet)
