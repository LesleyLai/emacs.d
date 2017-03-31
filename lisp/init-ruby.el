;; Rbenv for ruby version manage
;; {{
(require-package 'rbenv)
(global-rbenv-mode)
(rbenv-use-global)
;; }}

;; Seeing is believing for Ruby buffer interaction {{
(require-package 'seeing-is-believing)
(setq seeing-is-believing-prefix "C-.")
(add-hook 'ruby-mode-hook 'seeing-is-believing)
(require 'seeing-is-believing)
;; }}

;; Ruby inferior REPL {{
(require-package 'inf-ruby)
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
;; }}

;; Test current file directly
;; {{
(require-package 'ruby-test-mode)
(require 'ruby-test-mode)
(add-hook 'ruby-mode-hook 'ruby-test-mode)

;; http://worace.works/2016/06/07/getting-started-with-emacs-for-ruby/
(add-hook 'compilation-finish-functions
          (lambda (buf strg)
            (switch-to-buffer-other-window "*compilation*")
            (read-only-mode)
            (goto-char (point-max))
            (local-set-key (kbd "q")
                           (lambda () (interactive) (quit-restore-window)))))
;; }}

;; auto-matching
(require-package 'ruby-electric)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

;; Robe mode
;; {{
(require-package 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
;; }}


(provide 'init-ruby)
