(defconst my-org-electric-pairs
  '((?/ . ?/)
    (?= . ?=)
    (?~ . ?~)
    (?$ . ?$))
  "Electric pairs for org-mode.")

(defun my-org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs my-org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

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
         ("M-d" . org-delete-backward-char)
         ("C-c e" . my/org-toggle-emphasis)
         ("C-S-i" . hydra-move-lines/move-lines-up)
         ("C-S-k" . hydra-move-lines/move-lines-down)
         ("C-S-<up>" . hydra-move-lines/move-lines-up)
         ("C-S-<down>" . hydra-move-lines/move-lines-down))
  :config
  (progn
    (defvar org-setting-file "~/Dropbox/org/org.el")
    (if (file-exists-p org-setting-file) (load-file org-setting-file)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (js . t)
     (C . t)
     (rust . t)
     (python . t)
     (gnuplot . t)
     (clojure . t)))

  (dolist (face '((org-document-title . 1.5)
                  (org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.05)
                  (org-headline-done . 1.05)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face) :foreground "black"))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-headline-done nil :inherit 'variable-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)

  ;; Toggle for emphasis
  (defun my/org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t))
  (org-mode-restart))

  (major-mode-hydra-define org-mode (:quit-key "q" :color pink)
    ("Navigation"
     (("k" outline-next-visible-heading "next heading")
      ("i" outline-previous-visible-heading "prev heading")
      ("K" org-forward-heading-same-level "next heading at same level")
      ("I" org-backward-heading-same-level "prev heading at same level")
      ("j" outline-up-heading "up heading")
      ("g" org-goto "goto" :exit t))
     "Editing"
     (("x" org-cut-subtree "cut subtree")
      ("c" org-copy-subtree "copy subtree")
      ("v" org-paste-subtree "paste subtree")
      ("^" org-sort "sort")
      ("f" org-ctrl-c-ctrl-c "ctrl-c ctrl-c")
      ("*" org-toggle-heading "toggle heading")
      ("-" org-ctrl-c-minus "toggle item")
      ("~" org-table-create "create table"))
     "Refile"
     (("a" org-archive-subtree "archive subtree")
      ("w" org-refile "refile"))
     "Toggle"
     (("ti" org-toggle-inline-images "Image" :toggle org-inline-image-overlays)
      ("tt" org-latex-preview "Latex")
      ("tl" org-toggle-link-display "Link Descriptive" :toggle org-link-descriptive)
      ("tg" grip-mode "Github Readme Preview" :toggle t)
      ("te" my/org-toggle-emphasis "Hide Emphasis" :toggle org-hide-emphasis-markers)
      ("tu" org-transclusion-mode "org-transclusion" :toggle t))
     "Babel"
     (("'" org-edit-src-code "Edit source"))))

  ;; https://emacs.stackexchange.com/questions/19843/how-to-automatically-adjust-an-org-task-state-with-its-children-checkboxes
  (defun my/org-checkbox-todo ()
    "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
    (let ((todo-state (org-get-todo-state)) beg end)
      (unless (not todo-state)
        (save-excursion
          (org-back-to-heading t)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (goto-char beg)
          (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                                 end t)
              (if (match-end 1)
                  (if (equal (match-string 1) "100%")
                      (unless (string-equal todo-state "DONE")
                        (org-todo 'done))
                    (unless (string-equal todo-state "TODO")
                      (org-todo 'todo)))
                (if (and (> (match-end 2) (match-beginning 2))
                         (equal (match-string 2) (match-string 3)))
                    (unless (string-equal todo-state "DONE")
                      (org-todo 'done))
                  (unless (string-equal todo-state "TODO")
                    (org-todo 'todo)))))))))
  (add-hook 'org-checkbox-statistics-hook 'my/org-checkbox-todo)

  :custom
  (org-src-tab-acts-natively t)
  (org-agenda-span 'day)
  (org-image-actual-width nil)
  (org-edit-src-content-indentation 0)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)
  (org-confirm-babel-evaluate nil)
  (org-format-latex-header
   (concat org-format-latex-header
           "\n\\newcommand{\\inner}[2]{\\langle #1, #2 \\rangle}"
           "\n\\newcommand{\\innerf}[2]{\\langle\\langle #1, #2 \\rangle\\rangle}")
   )
  (org-babel-clojure-backend 'cider)

  :hook ((org-mode . visual-line-mode)
         (org-mode . (lambda () (variable-pitch-mode 1)))
         ;; Inhibit < for electric-pair mode
         (org-mode . (lambda ()
                       (setq-local electric-pair-inhibit-predicate
                                   `(lambda (c)
                                      (if (char-equal c ?<)
                                          t
                                        (,electric-pair-inhibit-predicate c))))))
         (org-mode . my-org-add-electric-pairs)))

;; An outline of pretty bullets instead of a list of asterisks.
(use-package org-bullets
  :after org
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package org-roam
  :ensure t
  :diminish
  :after org
  :hook
  (after-init . org-roam-mode)
  :config
  (require 'org-roam-protocol)
  (defhydra hydra-org-roam-meta(:color amaranth)
    "Org Roam Meta"
    ("t" org-roam-tag-add "Add roam tag")
    ("a" org-roam-alias-add "Add alias")
    ("q" nil "Exit" :exit t))
    :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))
              (("C-c n t" . hydra-org-roam-meta/org-roam-tag-add)))
  :ryo
  ("SPC n" (("l" org-roam)
            ("f" org-roam-find-file)
            ("g" org-roam-graph))
   :name "note taking")
  (:mode 'org-mode)
    ("SPC n" (("i" org-roam-insert)
              ("I" org-roam-insert-immediate)
              ("t" hydra-org-roam-meta/org-roam-tag-add))))

(use-package deft
  :ensure t
  :after org
  :bind
  ("C-c n d" . deft)
  :ryo
  ("SPC n d" deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org"))

(use-package org-journal
  :ensure t
  :after org
  :bind
  ("C-c n j" . org-journal-new-entry)
  :ryo
  ("SPC n j" org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration t))

(use-package org-roam-server
  :defer t
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 20213
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
  :defer t
  :after org
  :ensure t)

(use-package org-download
  :after org
  :ensure t
  :config
  ;; add support to dired
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-roam-bibtex
  :ensure t
  :diminish
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
              (("C-c n a" . orb-note-actions)))
  :ryo
  ("SPC n a" orb-note-actions)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-field t
        orb-file-field-extensions "pdf")

  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${citekey}"
           :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:"))))

(use-package org-edna
  :ensure t
  :after org
  :diminish
  :config
  (org-edna-mode))


(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode))

;; Exporters
(use-package ox-gfm
  :ensure t
  :defer 3
  :after org)

;; org-transclusion
(use-package org-transclusion
  :after org
  :load-path "site-lisp/org-transclusion"
  :custom
  (org-transclusion-activate-persistent-message nil)
  :ryo
  (:mode 'org-transclusion-mode)
  ("SPC n" (("e" org-transclusion-open-edit-src-buffer-at-point)
            ("o" org-transclusion-open-src-buffer-at-point))))

(use-package ob-rust
  :ensure t)

(provide 'init-org)
