(use-package org
  :defer t
  :ensure org-plus-contrib
  :init
  :bind ("C-x a" . org-agenda)
  :config
  (progn
    (defun org-mode-hook-setup ()
      (flyspell-mode 1)

      ;; display wrapped lines instead of truncated lines
      (visual-line-mode 1))

    (add-hook 'org-mode-hook 'org-mode-hook-setup)
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
    ))

                                        ; An outline of pretty bullets instead of a list of asterisks.
(use-package org-bullets
  :after org
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(provide 'init-org)
