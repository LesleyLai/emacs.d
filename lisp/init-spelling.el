;;; init-spelling.el --- Spell checking

(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

(provide 'init-spelling)
