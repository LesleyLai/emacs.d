;; js-mode
(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook (lambda () (customize-set-variable 'js-indent-level 2)))))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :interpreter ("node" . js2-mode)
  :bind (("C-a" . back-to-indentation-or-beginning-of-line)
         ("C-M-h" . backward-kill-word))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
(bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map)))))

(provide 'init-javascript)
