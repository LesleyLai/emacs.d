;; Racket mode
(use-package racket-mode
  :ensure t
  :defer t
  :hook
  (racket-repl-mode
   . (lambda ()
       (define-key racket-repl-mode-map (kbd "C-w") 'kill-current-buffer))))

;; racket-repl-mode-map

(provide 'init-racket)
