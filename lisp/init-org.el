(use-package org
  :defer t
  :ensure org-plus-contrib
  :init
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  :bind (("C-x a" . org-agenda)
         :map org-mode-map
         ("M-J" . org-shiftleft)
         ("M-L" . org-shiftright)
         ("M-K" . org-shiftdown)
         ("M-I" . org-shiftup)
         ("M-d" . org-delete-backward-char))
  :config
  (progn
    (defvar org-setting-file "~/Dropbox/org/org.el")
    (if (file-exists-p org-setting-file) (load-file org-setting-file))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (js . t)
       (C . t))))

  :custom
  (org-modules
   (quote
    (org-bbdb org-bibtex org-ctags
              org-docview org-gnus org-habit
              org-info org-irc org-mhe
              org-rmail org-w3m)))
  (org-src-tab-acts-natively t)
  (org-image-actual-width nil)
  :hook ((org-mode . flyspell-mode)
         (org-mode . visual-line-mode)))

;; An outline of pretty bullets instead of a list of asterisks.
(use-package org-bullets
  :after org
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package org-roam
  :ensure t
  :after modalka
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
  :config
  (define-key modalka-mode-map (kbd "SPC n l") #'org-roam)
  (define-key modalka-mode-map (kbd "SPC n f") #'org-roam-find-file)
  (define-key modalka-mode-map (kbd "SPC n g") #'org-roam-graph)
  (define-key modalka-mode-map (kbd "SPC n i") #'org-roam-insert)
  (define-key modalka-mode-map (kbd "SPC n I") #'org-roam-insert-immediate)
  :config
  (require 'org-roam-protocol))

(use-package deft
  :ensure t
  :after org modalka
  :bind
  ("C-c n d" . deft)
  :config
  (define-key modalka-mode-map (kbd "SPC n d") #'deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org"))

(use-package org-journal
  :ensure t
  :after org modalka
  :bind
  ("C-c n j" . org-journal-new-entry)
  :config
  (define-key modalka-mode-map (kbd "SPC n j") #'org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package company-org-roam
  :ensure t
  :after org-roam
  :config
  (push 'company-org-roam company-backends))

(use-package org-books
  :after org
  :ensure t)

(use-package org-download
  :ensure t
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-noter
  :after org
  :ensure t
  :config
  (setq org-noter-notes-search-path '("~/Dropbox/org/roam")))

(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
        org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
        org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"))

(provide 'init-org)
