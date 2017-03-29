(provide 'init-flycheck)

(add-hook 'prog-mode-hook 'global-flycheck-mode)

(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-cstyle)
     (flycheck-cstyle-setup)
     ;; chain after cppcheck since this is the last checker in the upstream
     ;; configuration
     (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))))
