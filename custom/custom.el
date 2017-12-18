(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/auto-save-list/" t))))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(company-idle-delay 0.1)
 '(company-tooltip-align-annotations t)
 '(confirm-nonexistent-file-or-buffer nil)
 '(csv-separators (quote ("," ";" "|" " ")))
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (qtcreator)))
 '(custom-safe-themes
   (quote
    ("694dab4e0ec836cb4611b7e719c1acdc6e768f29026254074c80c5d9604b8907" default)))
 '(dashboard-banner-logo-title "Welcome Lesley :-)" t)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions -1)
 '(frame-title-format (quote ("%b " "%[ - GNU %F " "25.1.1")) t)
 '(git-gutter:handled-backends (quote (svn hg git)))
 '(history-delete-duplicates t)
 '(history-length t)
 '(icon-title-format (quote ("%b " " - GNU %F " "25.1.1")) t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-use-faces nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(kill-buffer-query-functions nil t)
 '(load-prefer-newer t)
 '(markdown-command "multimarkdown" t)
 '(org-agenda-custom-commands
   (quote
    (("W" "Weekly Review"
      ((agenda "l"
               ((org-agenda-ndays 7)
                (org-agenda-start-day "0d")
                (org-agenda-show-log t)))
       (stuck "")
       (todo "SOMEDAY")
       (todo "MAYBE"))))))
 '(org-agenda-show-future-repeats nil)
 '(org-agenda-start-on-weekday 0)
 '(org-capture-templates
   (quote
    (("t" "Todo [inbox]" entry
      (file+headline "~/Dropbox/org/inbox.org" "Tasks")
      "* TODO %i%?")
     ("T" "Tickler" entry
      (file+headline "~/Dropbox/org/inbox.org" "Tickler")
      "* %i%? 
 %T")
     ("M" "Maybe" entry
      (file+headline "~/Dropbox/org/inbox.org" "Maybe")
      "* MAYBE %i%?")
     ("S" "Someday" entry
      (file+headline "~/Dropbox/org/inbox.org" "Someday")
      "* SOMEDAY %i%?"))) t)
 '(org-confirm-babel-evaluate nil)
 '(org-default-notes-file "~/Dropbox/org/inbox.org")
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-ctags org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-refile-targets
   (quote
    (("~/Dropbox/org/tasks.org" :maxlevel . 3)
     ("~/Dropbox/org/assignments.org" :maxlevel . 3)
     ("~/Dropbox/org/inbox.org" :level . 1)
     ("~/Dropbox/org/blog.org" :maxlevel . 2)
     ("~/Dropbox/org/life.org" :maxlevel . 2)
     ("~/Dropbox/org/mooc.org" :maxlevel . 2))))
 '(org-stuck-projects
   (quote
    ("+LEVEL=2/-STARTED-SOMEDAY-MAYBE-WAITING-DONE" nil nil "SCHEDULED:\\|DEADLINE:")))
 '(org-tag-alist
   (quote
    (("@school" . 115)
     ("Mooc" . 109)
     ("Habit" . 72)
     ("Health" . 104)
     ("Dev" . 100)
     ("@home" . 104)
     ("Exercise" . 101)
     ("Reading" . 114))))
 '(package-archive-priorities (quote (("melpa" . 1))))
 '(package-selected-packages
   (quote
    (exec-path-from-shell htmlize tide typescript-mode ox-gfm ox-epub ggtags multiple-cursors git-gutter-fringe git-gutter magit switch-window esup sudo-edit idle-require undo-tree artbollocks-mode sr-speedbar dashboard glsl-mode yaml-mode web-mode use-package-chords sml-mode smex smart-mode-line-powerline-theme seeing-is-believing ruby-test-mode ruby-electric robe rjsx-mode rbenv rainbow-mode rainbow-delimiters racket-mode py-autopep8 pip-requirements paredit org-plus-contrib modern-cpp-font-lock markdown-mode ido-ubiquitous idle-highlight-mode flycheck-yamllint flycheck-cstyle flx-ido fill-column-indicator ensime dired+ csv-mode counsel company-web company-tern company-c-headers company-anaconda cmake-ide clang-format)))
 '(savehist-additional-variables (quote (search kill-ring search-ring regexp-search-ring)))
 '(savehist-file "~/.emacs.d/history")
 '(savehist-save-minibuffer-history 1)
 '(seeing-is-believing-prefix "C-.")
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(vc-make-backup-files t)
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
