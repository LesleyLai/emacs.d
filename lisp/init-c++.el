;; Modern c++ syntax highlighter
(use-package modern-cpp-font-lock :ensure t)

(use-package c++-mode
  :after  modern-cpp-font-lock
  :init
  (modern-c++-font-lock-global-mode t)
)

(provide 'init-c++)
