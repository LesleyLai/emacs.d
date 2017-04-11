(use-package org
  :ensure org-plus-contrib
  :config
  (progn
    (defun org-mode-hook-setup ()
      (flyspell-mode 1)

      ;; display wrapped lines instead of truncated lines
      (visual-line-mode 1))
    
    (add-hook 'org-mode-hook 'org-mode-hook-setup)))
  
(provide 'init-org)
