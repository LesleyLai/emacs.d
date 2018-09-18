;; Menubar
(define-key-after global-map
  [menu-bar file dired]
  '("Directory manager" . dired)
  'kill-buffer)

;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure nil
  :commands (dired-sidebar-toggle-sidebar))

(provide 'init-dired)
