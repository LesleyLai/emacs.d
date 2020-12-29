(use-package markdown-mode
  :ensure t
  :after major-mode-hydra
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (customize-set-variable 'markdown-command "multimarkdown")
  :config
  (major-mode-hydra-define markdown-mode (:quit-key "q" :color pink)
    ("Toggle"
     (("g" grip-mode "Github Readme Preview" :toggle t)))))

;; pip install grip
;; Need to set grip-github-user and grip-github-password
(use-package grip-mode
  :ensure t
  :custom
  (grip-preview-use-webkit nil))

(provide 'init-markdown)
