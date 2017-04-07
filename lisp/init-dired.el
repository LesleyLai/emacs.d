;; Dired+
(use-package dired+
  :ensure t)

;; Menubar
(define-key-after global-map
  [menu-bar file dired]
  '("Directory manager" . dired)
  'kill-buffer)

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

(provide 'init-dired)
