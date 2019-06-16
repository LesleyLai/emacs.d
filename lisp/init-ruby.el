;; Rbenv for ruby version manage
(use-package rbenv
  :ensure t
  :defer t
  :config
  (global-rbenv-mode)
  (rbenv-use-global))

;; Seeing is believing for Ruby buffer interaction
(use-package seeing-is-believing
  :ensure t
  :defer t
  :init
  (customize-set-variable 'seeing-is-believing-prefix "C-."))

;; Ruby inferior REPL
(use-package inf-ruby
  :ensure t
  :defer t
  :config (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;; Test current file directly
(use-package ruby-test-mode
  :ensure t
  :defer t
  :config
  (add-hook 'ruby-mode-hook 'ruby-test-mode)

  ;; http://worace.works/2016/06/07/getting-started-with-emacs-for-ruby/
  (add-hook 'compilation-finish-functions
            (lambda (buf strg)
              (switch-to-buffer-other-window "*compilation*")
              (read-only-mode)
              (goto-char (point-max))
              (local-set-key (kbd "q")
                             (lambda () (interactive) (quit-restore-window))))))

;; auto-matching
(use-package ruby-electric
  :ensure t
  :defer t
  :config
  (add-hook 'ruby-mode-hook 'ruby-electric-mode))

;; Robe mode
(use-package robe
  :ensure t
  :defer 3
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

(provide 'init-ruby)
