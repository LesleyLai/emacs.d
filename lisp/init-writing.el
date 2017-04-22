;;; init-writing.el --- Misc setting for writing

;; Bad words checker
(use-package artbollocks-mode
  :ensure t
  :config
  (add-hook 'text-mode-hook 'artbollocks-mode)
  (add-hook 'org-mode-hook 'artbollocks-mode)

  (setq artbollocks-weasel-words-regex
      (concat "\\b" (regexp-opt
                     '("one of"
                       "should"
                       "just"
                       "sort of"
                       "a lot"
                       "a bit"
                       "a few"
                       "kind of"
                       "sort of"
                       "many"
                       "several"
                       "needless to say"
                       "clearly"
                       "naturally"
                       "obviously"
                       "of course"
                       "probably"
                       "actually"
                       "arguably"
                       "basically"
                       "essentially"
                       "commonly"
                       "totally"
                       "remarkably"
                       "largely"
                       "interestingly"
                       "significantly"
                       "substantially"
                       "relatively"
                       "completely"
                       "quite"
                       "perhaps"
                       "serves to"
                       "helps to"
                       "really"
                       "pretty"
                       "extremely"
                       "fairly"
                       "maybe"
                       "just"
                       "nice"
                       "good"
                       "stuff"
                       "so"
                       "thing"
                       "things"
                       "utilize"
                       "utilizes"
                       "turns out that"
                       "it is said"
                       "very"
                       "in conclusion"
                       
                       ) t) "\\b"))

  (setq artbollocks-jargon nil)
  
  ;; Fix a bug in the regular expression to catch repeated words
  (setq lexical-illusions-regex "\\b\\(\\w+\\)\\W+\\(\\1\\)\\b")

  ;; Make sure keywords are case-insensitive
  (defadvice artbollocks-search-for-keyword (around sacha activate)
    "Match in a case-insensitive way."
    (let ((case-fold-search t))
      ad-do-it)))



(provide 'init-writing)
;;; init-writing.el ends here
