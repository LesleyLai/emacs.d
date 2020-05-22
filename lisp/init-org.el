(use-package org
  :defer t
  :ensure org-plus-contrib
  :init
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  :bind ("C-x a" . org-agenda)
  :config
  (progn
    (defvar org-setting-file "~/Dropbox/org/org.el")
    (if (file-exists-p org-setting-file) (load-file org-setting-file))

    (customize-set-variable 'org-modules
                            (quote
                             (org-bbdb org-bibtex org-ctags
                                       org-docview org-gnus org-habit
                                       org-info org-irc org-mhe
                                       org-rmail org-w3m)))
    (customize-set-variable 'org-src-tab-acts-natively t)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (js . t)
       (C . t)))
    )

  :hook ((org-mode . flyspell-mode)
         (org-mode . visual-line-mode))

  )

; An outline of pretty bullets instead of a list of asterisks.
(use-package org-bullets
  :after org
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(provide 'init-org)
