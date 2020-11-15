(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'"
  :custom
  (csv-separators '("," ";" "|" " ")))

(provide 'init-csv)
