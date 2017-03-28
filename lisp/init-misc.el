(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
                  
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Hook for programming
(defun generic-programming-mode-hook-setup ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (idle-highlight-mode t))

(add-hook 'prog-mode-hook 'generic-programming-mode-hook-setup)
(add-hook 'css-mode-hook 'generic-programming-mode-hook-setup)

(provide 'init-misc)
