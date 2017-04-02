;; Modern c++ syntax highlighter
(use-package modern-cpp-font-lock :ensure t)

(defun c++-mode-hook-setup ()
  (modern-c++-font-lock-global-mode t)
  )

(add-hook 'c++-mode-hook 'c++-mode-hook-setup)

(provide 'init-c++)
