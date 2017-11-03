;; js-mode
(use-package js
  :config
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(provide 'init-javascript)
