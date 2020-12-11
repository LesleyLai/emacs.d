;; Menubar
(define-key-after global-map
  [menu-bar file dired]
  '("Dired" . dired)
  'kill-buffer)

(use-package dired
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-gaGh"))
  :bind (:map dired-mode-map
              ("i" . dired-previous-line)
              ("k" . dired-next-line)
              ("j" . dired-single-up-directory)
              ("l" . dired-single-buffer))
  :hook (dired-mode . auto-revert-mode))

;; This package makes possible to use the same buffer to navigate in dired without creating new buffer
(use-package dired-single
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode))


(provide 'init-dired)
