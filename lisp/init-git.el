;; ============================================
;; ui for git
;; ============================================
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Git gutter
(use-package git-gutter
  :ensure t
  :init
  (if *use-GUI*
      (use-package git-gutter-fringe :ensure t)
    (git-gutter:linum-setup))
  (global-git-gutter-mode)
  (custom-set-variables '(git-gutter:handled-backends '(svn hg git))))


(provide 'init-git)
